import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as FunTypes from "./rts.funtypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";
import { stg_arg_bitmaps } from "./rts.autoapply.mjs";

function bdescr(c) {
  const nc = Number(c);
  return nc - (nc & (rtsConstants.mblock_size - 1)) + rtsConstants.offset_first_bdescr;
}

/**
 * Class implementing copying garbage collection.
 */
export class GC {
  constructor(
    memory,
    heapalloc,
    stableptr_manager,
    stablename_manager,
    scheduler,
    info_tables,
    export_stableptrs,
    symbol_table,
    reentrancy_guard,
    yolo,
    gcThreshold
  ) {
    this.memory = memory;
    this.heapAlloc = heapalloc;
    this.stablePtrManager = stableptr_manager;
    this.stableNameManager = stablename_manager;
    this.scheduler = scheduler;
    this.infoTables = info_tables;
    for (const p of export_stableptrs) this.stablePtrManager.newStablePtr(p);
    this.symbolTable = symbol_table;
    this.reentrancyGuard = reentrancy_guard;
    /**
     * 'Yolo' mode disables garbage collection altogether
     * (see {@link GC#performGC})
     * @name GC#yolo
     */
    this.yolo = yolo;
    /**
     * Garbage collection will not be performed when the
     * current number of "live" MBlocks is less than
     * {@link GC#gcThreshold} (see {@link GC#performGC}).
     * @name GC#gcThreshold
     * @default 64
     */
    this.gcThreshold = gcThreshold;
    /**
     * Set of closures encountered during garbage
     * collection but not moved: they are either
     * closures in the statis part of memory, or 
     * closures in pinned MBlocks.
     * @name GC#nonMovedObjects
     */
    this.nonMovedObjects = new Set();
    /**
     * Set containing the MBlocks in the to-space,
     * i.e. the MBlocks where reachable objects are copied
     * during garbage collection.
     * Notes:
     * 1) Pinned MBlocks are not copied during GC: they are
     *    simply set as live, and added to the liveMBlocks set.
     * 2) Static objects are not copied either, but their
     *    blocks are not even added to the liveMBlocks set.
     * @name GC#liveMBlocks
     */
    this.liveMBlocks = new Set();
    /**
     * Set containing the MBlocks in the from-space,
     * i.e. the MBlocks that have containing objects
     * that have been copied into to-space. These MBlocks
     * will be freed at the end of garbage collection.
     * @name GC#deadMBlocks
     */
    this.deadMBlocks = new Set();
    /**
     * A work list where evacuated objects are pushed
     * by {@link GC#evacuateClosure} in order to be later
     * scavenged by {@link GC#scavengeWorkList}.
     * @name GC#workList
     */
    this.workList = [];
    /**
     * At each garbage collection, the live JSVals encountered are
     * recorded in {@link GC#liveJSVals}, and then handled separately 
     * by {@link StablePtrManager}.
     * @name GC#liveJSVals
     */
    this.liveJSVals = new Set();
    Object.freeze(this);
  }

  /**
   * Checks whether the provided memory address resides
   * in a pinned MBlock. Used by {@link GC#evacuateClosure}
   * to avoid evacuating pinned objects.
   * @param addr The memory address to check
   */
  isPinned(addr) {
    const bd = bdescr(addr),
      flags = this.memory.i16Load(bd + rtsConstants.offset_bdescr_flags);
    return Boolean(flags & rtsConstants.BF_PINNED);
  }

  /**
   * Heap alloactes a physical copy of the given closure.
   * Used during evacuation by {@link GC#evacuateClosure}.
   * @param c The source address of the closure
   * @param bytes The size in bytes of the closure 
   */
  copyClosure(c, bytes) {
    const dest_c = this.heapAlloc.allocate(Math.ceil(bytes / 8));
    this.memory.memcpy(dest_c, c, bytes);
    this.liveMBlocks.add(bdescr(dest_c));
    this.deadMBlocks.add(bdescr(c));
    return dest_c;
  }

/**
 * Performs _stingy_ evaluation, i.e. a very frugual form
 * of evaluation that is carried during garbage collection.
 * It implements the following two optimizations:
 * - Indirections short-cutting;
 * - Selector optimization: remove thunks of applications of field
 *   selectors.
 * Only the argument `c` is required: the other arguments will be
 * computed when `undefined`.
 * @param c - The address of the closure
 * @param {number=} untagged_c - The unDynTag-ed address
 * @param {number=} info - The info pointer of `c`
 * @param {number=} type - The closure type of `c`
 * @returns A tuple array `[header, type]` where `header` is the header
 *   of the closure `c` after evaluation, and `type` is the resulting
 *   closure type.
 */
  stingyEval(c, untagged_c, info, type) {
    const stack = [arguments];
    // This function proceeds by traversing IND and THUNK_SELECTOR
    // closures, until it finds a result. It proceeds in two phases:
    // - Search phase: indirectee/selectee pointers are followed,
    //     and the closures encountered are pushed on a stack
    // - Back propagation phase: the closures on the stack are popped
    //     and replaced with IND closures where possible.
    // Note: the two phases can be interleaved. 
    let result, peek;
    while (stack.length) {
      if (!result) {
        // Search phase
        [c, untagged_c, info, type] = peek = stack[stack.length-1];
        if (!untagged_c) {
          // If no information about c is present, compute it
          const untagged_c = peek[1] = Memory.unDynTag(c),
            info = peek[2] = Number(this.memory.i64Load(untagged_c));
          if (info % 2) {
            // The closures has already been overwritten with a 
            // forwarding pointer: switch phase.
            result = stack.pop(); // = peek
            continue;
          }
          type = peek[3] = this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_type);
        }
        switch (type) {
          case ClosureTypes.IND: {
            // Whitehole
            this.memory.i64Store(untagged_c, this.symbolTable.stg_WHITEHOLE_info);
            // Follow the indirectee
            stack.push([this.memory.i64Load(untagged_c + rtsConstants.offset_StgInd_indirectee),,,]);
            continue;
          }
          case ClosureTypes.THUNK_SELECTOR: {
            // Whitehole
            this.memory.i64Store(untagged_c, this.symbolTable.stg_WHITEHOLE_info);
            // Follow the selectee
            stack.push([this.memory.i64Load(untagged_c + rtsConstants.offset_StgSelector_selectee),,,]);
            continue;
          }
          default: {
            // Switch phase
            result = stack.pop();
            continue;
          }
        }
      } else {
        // Back propagation phase
        [c, untagged_c, info, type] = peek = stack.pop();
        let [res_c, _, res_info, res_type] = result;
        switch (type) {
          case ClosureTypes.IND: {
            if (res_info % 2) {
              this.memory.i64Store(untagged_c, res_info);
              info = res_info; type = res_type;
            } else {
              this.memory.i64Store(untagged_c, this.symbolTable.stg_IND_info); // Undo whiteholing
              this.memory.i64Store(untagged_c + rtsConstants.offset_StgInd_indirectee, res_c);
            }
            continue;
          }
          case ClosureTypes.THUNK_SELECTOR: {
            if (res_type == ClosureTypes.CONSTR) {
              const offset = this.memory.i32Load(
                res_info + rtsConstants.offset_StgInfoTable_layout
              );
              peek[2] = this.symbolTable.stg_IND_info;
              stack.push(peek); 
              // Start evaluating the selected field
              stack.push([res_c + offset,,,]);
              result = undefined;
              continue;
            } else {
              this.memory.i64Store(untagged_c, info); // Undo whiteholing
              if (res_info % 2 == 0)
                // Only if not a forwarding pointer: if the result has already
                // been evacuated, leave everything as it is, and the
                // evacuation function will take care of it later
                this.memory.i64Store(untagged_c + rtsConstants.offset_StgSelector_selectee, res_c); 
              // Continue backtracking
              result = peek;
              continue;
            }
          }
          // No other options for the switch: only IND or THUNK_SELECTOR
          // closures are pushed to the stack
          default: throw WebAssembly.RuntimeError(`Unexpected closure type: ${type}`);
        }
      }
    }
    return [info, type];
  }

  /**
   * Evacuates a closure. This consists of: 
   * (1) Copying the closure into to-space through {@link GC#copyClosure}
   * (2) Map the old unDynTag-ed address of the closure
   *     to its new unDynTag-ed address in {@link GC#closureIndirects}.
   * If that closure had already been evacuated, simply
   * return the forwarding pointer already present in {@link GC#closureIndirects}.
   * @param c The memory address of the closure to evacuate.
   */
  evacuateClosure(c) {
    if (!Memory.getTag(c)) {
      // c is the address of a JSVal
      if (!(Number(c) & 1))
        throw new WebAssembly.RuntimeError(`Illegal JSVal 0x${c.toString(16)}`);
      this.liveJSVals.add(Number(c));
      return c;
    }
    const tag = Memory.getDynTag(c),
      untagged_c = Memory.unDynTag(c);
    let info = Number(this.memory.i64Load(untagged_c));

    if (info % 2) {
      // The info header has already been overwritten with
      // a forwarding address: just follow it
      return Memory.setDynTag(info, tag);
    } else if (this.nonMovedObjects.has(untagged_c)) {
      // The closure is eiter pinned or static, and has
      // already been enqueued for scavenging: just return it
      return c;
    } else if (!this.memory.heapAlloced(untagged_c)) {
      // Object in the static part of the memory:
      // it won't be copied ...
      this.nonMovedObjects.add(untagged_c);
      // ... but it will still be scavenged
      this.workList.push(untagged_c);
      // Warning: do not set the MBlock as live,
      // because the static part of memory is not
      // tracked by HeapAlloc.mgroups and it would
      // break the checks in HeapAlloc.handleLiveness.
      return c;
    } else if (this.isPinned(untagged_c)) {
      // The object belongs to a pinned MBlock:
      // it won't be copied ...
      this.nonMovedObjects.add(untagged_c);
      // ... but it will still be scavenged
      this.workList.push(untagged_c);
      // Set the pinned MBlock as live
      this.liveMBlocks.add(bdescr(untagged_c));
      return c;
    }
    // The closure is heap-allocated and dynamic:
    // proceed to evacuate it into to-space
    if (this.infoTables && !this.infoTables.has(info))
      throw new WebAssembly.RuntimeError(
        `Invalid info table 0x${info.toString(16)}`
      );
    let dest_c = undefined;
    // Get the type of the closure from info tables
    let type = this.memory.i32Load(
      info + rtsConstants.offset_StgInfoTable_type
    );
    if (type == ClosureTypes.THUNK_SELECTOR || type == ClosureTypes.IND) {
      // Optimize selectors and indirections
      [info, type] = this.stingyEval(c, untagged_c, info, type);
      // The info header has already been overwritten with
      // a forwarding address: just follow it
      if (info % 2)
        return Memory.setDynTag(info, tag);
    }
    switch (type) {
      case ClosureTypes.CONSTR_0_1:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.CONSTR_1_0: {
        dest_c = this.copyClosure(untagged_c, 16);
        break;
      }
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1: {
        dest_c = this.copyClosure(untagged_c, rtsConstants.sizeof_StgThunk + 8);
        break;
      }
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_0_2: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgThunk + 16
        );
        break;
      }
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.CONSTR_1_1:
      case ClosureTypes.CONSTR_2_0:
      case ClosureTypes.CONSTR_0_2: {
        dest_c = this.copyClosure(untagged_c, 24);
        break;
      }
      case ClosureTypes.THUNK: {
        const ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout
          ),
          non_ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout + 4
          );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgThunk + ((ptrs + non_ptrs) << 3)
        );
        break;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_NOCAF:
      case ClosureTypes.MVAR_CLEAN:
      case ClosureTypes.MVAR_DIRTY:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.WEAK:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.BLACKHOLE: {
        const ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout
          ),
          non_ptrs = this.memory.i32Load(
            info + rtsConstants.offset_StgInfoTable_layout + 4
          );
        dest_c = this.copyClosure(untagged_c, (1 + ptrs + non_ptrs) << 3);
        break;
      }
      case ClosureTypes.THUNK_SELECTOR: {
        dest_c = this.copyClosure(untagged_c, rtsConstants.sizeof_StgSelector);
        break;
      }
      case ClosureTypes.IND: {
        dest_c = this.evacuateClosure(
          this.memory.i64Load(
            untagged_c + rtsConstants.offset_StgInd_indirectee
          )
        );
        // cannot simply break here, because dest_c must not
        // be pushed to this.workList since it has already
        // been evacuated above
        this.memory.i64Store(untagged_c, dest_c + 1);
        return dest_c;
      }
      case ClosureTypes.PAP: {
        const n_args = this.memory.i32Load(
          untagged_c + rtsConstants.offset_StgPAP_n_args
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgPAP + (n_args << 3)
        );
        break;
      }
      case ClosureTypes.AP: {
        const n_args = this.memory.i32Load(
          untagged_c + rtsConstants.offset_StgAP_n_args
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgAP + (n_args << 3)
        );
        break;
      }
      case ClosureTypes.AP_STACK: {
        const size = Number(
          this.memory.i64Load(untagged_c + rtsConstants.offset_StgAP_STACK_size)
        );
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgAP_STACK + (size << 3)
        );
        break;
      }
      case ClosureTypes.ARR_WORDS: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgArrBytes +
            Number(
              this.memory.i64Load(
                untagged_c + rtsConstants.offset_StgArrBytes_bytes
              )
            )
        );
        break;
      }
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgMutArrPtrs +
            (Number(
              this.memory.i64Load(
                untagged_c + rtsConstants.offset_StgMutArrPtrs_ptrs
              )
            ) <<
              3)
        );
        break;
      }
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        dest_c = this.copyClosure(
          untagged_c,
          rtsConstants.sizeof_StgSmallMutArrPtrs +
            (Number(
              this.memory.i64Load(
                untagged_c + rtsConstants.offset_StgSmallMutArrPtrs_ptrs
              )
            ) <<
              3)
        );
        break;
      }
      default:
        throw new WebAssembly.RuntimeError();
    }
    // Overwrite the object header with a forwarding
    // pointer (i.e. store the address with the
    // least significant bit set to 1)
    this.memory.i64Store(untagged_c, dest_c + 1);
    // Enqueue the destination object in the workList,
    // so that it will be scavenged later
    this.workList.push(dest_c);
    // Finally, return the new address
    return Memory.setDynTag(dest_c, tag);
  }

  scavengeClosureAt(p) {
    this.memory.i64Store(p, this.evacuateClosure(this.memory.i64Load(p)));
  }

  scavengePointersFirst(payload, ptrs) {
    for (let i = 0; i < ptrs; ++i) this.scavengeClosureAt(payload + (i << 3));
  }

  scavengeSmallBitmap(payload, bitmap, size) {
    for (let i = 0; i < size; ++i)
      if (!(Number(bitmap >> BigInt(i)) & 1))
        this.scavengeClosureAt(payload + (i << 3));
  }

  scavengeLargeBitmap(payload, large_bitmap, size) {
    for (let j = 0; j < size; j += 64) {
      const bitmap = this.memory.i64Load(
        large_bitmap + rtsConstants.offset_StgLargeBitmap_bitmap + (j >> 3)
      );
      for (let i = j; i - j < 64 && i < size; ++i)
        if (!(Number(bitmap >> BigInt(i - j)) & 1))
          this.scavengeClosureAt(payload + (i << 3));
    }
  }

  scavengePAP(c, offset_fun, payload, n_args) {
    this.scavengeClosureAt(c + offset_fun);
    const fun = this.memory.i64Load(c + offset_fun),
      fun_info = Number(this.memory.i64Load(Memory.unDynTag(fun)));
    if (this.infoTables && !this.infoTables.has(fun_info))
      throw new WebAssembly.RuntimeError(
        `Invalid info table 0x${fun_info.toString(16)}`
      );
    switch (
      this.memory.i32Load(
        fun_info +
          rtsConstants.offset_StgFunInfoTable_f +
          rtsConstants.offset_StgFunInfoExtraFwd_fun_type
      )
    ) {
      case FunTypes.ARG_GEN: {
        this.scavengeSmallBitmap(
          payload,
          this.memory.i64Load(
            fun_info +
              rtsConstants.offset_StgFunInfoTable_f +
              rtsConstants.offset_StgFunInfoExtraFwd_b
          ) >> BigInt(6),
          n_args
        );
        break;
      }
      case FunTypes.ARG_GEN_BIG: {
        this.scavengeLargeBitmap(
          payload,
          Number(
            this.memory.i64Load(
              fun_info +
                rtsConstants.offset_StgFunInfoTable_f +
                rtsConstants.offset_StgFunInfoExtraFwd_b
            )
          ),
          n_args
        );
        break;
      }
      case FunTypes.ARG_BCO: {
        throw new WebAssembly.RuntimeError();
      }
      default: {
        this.scavengeSmallBitmap(
          payload,
          BigInt(
            stg_arg_bitmaps[
              this.memory.i32Load(
                fun_info +
                  rtsConstants.offset_StgFunInfoTable_f +
                  rtsConstants.offset_StgFunInfoExtraFwd_fun_type
              )
            ]
          ) >> BigInt(6),
          n_args
        );
        break;
      }
    }
  }

  scavengeStackChunk(sp, sp_lim) {
    let c = sp;
    while (true) {
      if (c > sp_lim) throw new WebAssembly.RuntimeError();
      if (c == sp_lim) break;
      const info = Number(this.memory.i64Load(c)),
        type = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_type
        ),
        raw_layout = this.memory.i64Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
      if (this.infoTables && !this.infoTables.has(info))
        throw new WebAssembly.RuntimeError(
          `Invalid info table 0x${info.toString(16)}`
        );
      if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
        this.evacuateClosure(
          this.memory.i64Load(info + rtsConstants.offset_StgRetInfoTable_srt)
        );
      switch (type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME: {
          const size = Number(raw_layout) & 0x3f,
            bitmap = raw_layout >> BigInt(6);
          this.scavengeSmallBitmap(c + 8, bitmap, size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(
            this.memory.i64Load(
              Number(raw_layout) + rtsConstants.offset_StgLargeBitmap_size
            )
          );
          this.scavengeLargeBitmap(c + 8, Number(raw_layout), size);
          c += (1 + size) << 3;
          break;
        }

        // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/rts/Printer.c#L609
        // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/rts/sm/Scav.c#L1944
        case ClosureTypes.RET_FUN: {
          const retfun = c;
          const size = Number(
            this.memory.i64Load(retfun + rtsConstants.offset_StgRetFun_size)
          );

          // NOTE: the order is important. The scavenging will move all the
          // data inside, so that when we grab "fun", we grab the right fun
          // that has been moved.
          this.scavengeClosureAt(retfun + rtsConstants.offset_StgRetFun_fun);
          let fun = Number(
            this.memory.i64Load(retfun + rtsConstants.offset_StgRetFun_fun)
          );
          const fun_info_p = fun + 0;
          const fun_info = Number(
            this.memory.i64Load(Memory.unDynTag(fun_info_p))
          );

          const fun_type = this.memory.i32Load(
            fun_info +
              rtsConstants.offset_StgFunInfoTable_f +
              rtsConstants.offset_StgFunInfoExtraFwd_fun_type
          );

          const ret_fun_payload =
            retfun + rtsConstants.offset_StgRetFun_payload;

          switch (fun_type) {
            case FunTypes.ARG_GEN: {
              this.scavengeSmallBitmap(
                c + rtsConstants.offset_StgRetFun_payload,
                this.memory.i64Load(
                  fun_info +
                    rtsConstants.offset_StgFunInfoTable_f +
                    rtsConstants.offset_StgFunInfoExtraFwd_b
                ) >> BigInt(6),
                size
              );
              break;
            }
            case FunTypes.ARG_GEN_BIG: {
              this.scavengeLargeBitmap(
                c + rtsConstants.offset_StgRetFun_payload,
                Number(
                  this.memory.i64Load(
                    fun_info +
                      rtsConstants.offset_StgFunInfoTable_f +
                      rtsConstants.offset_StgFunInfoExtraFwd_b
                  )
                ),
                size
              );
              break;
            }
            case FunTypes.ARG_BCO: {
              throw new WebAssembly.RuntimeError();
            }
            default: {
              // https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/Constants.h#L186
              const BITMAP_SIZE_MASK = 0x3f;
              const BITMAP_BITS_SHIFT = 6;
              const bitmap = stg_arg_bitmaps[fun_type];

              // https://github.com/ghc/ghc/blob/2ff77b9894eecf51fa619ed2266ca196e296cd1e/includes/rts/storage/InfoTables.h#L116
              const bitmap_bits = BigInt(bitmap) >> BigInt(BITMAP_BITS_SHIFT);
              const bitmap_size = bitmap & BITMAP_SIZE_MASK;

              this.scavengeSmallBitmap(
                ret_fun_payload,
                bitmap_bits,
                bitmap_size
              );

              break;
            } // end case default
          } //end switch (fun_type)
          c += rtsConstants.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError();
      }
    }
  }

  /**
   * Iterates over {@link GC#workList} and scavenges the enqueued objects.
   */
  scavengeWorkList() {
    while (this.workList.length) this.scavengeClosure(this.workList.pop());
  }

  /**
   * Scavenges a single object in to-space by evacuating
   * each pointer in the object, and replacing the pointer
   * with the address obtained after evacuation.
   * @param c The address of the closure to scavenge
   * @param info The info pointer of the closure
   */
  scavengeClosure(c) {
    const info = Number(this.memory.i64Load(c)),
      type = this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_type);
    if (this.infoTables && !this.infoTables.has(info))
      throw new WebAssembly.RuntimeError(
        `Invalid info table 0x${info.toString(16)}`
      );
    switch (type) {
      case ClosureTypes.CONSTR:
      case ClosureTypes.CONSTR_1_0:
      case ClosureTypes.CONSTR_0_1:
      case ClosureTypes.CONSTR_2_0:
      case ClosureTypes.CONSTR_1_1:
      case ClosureTypes.CONSTR_0_2:
      case ClosureTypes.CONSTR_NOCAF: {
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.FUN:
      case ClosureTypes.FUN_1_0:
      case ClosureTypes.FUN_0_1:
      case ClosureTypes.FUN_2_0:
      case ClosureTypes.FUN_1_1:
      case ClosureTypes.FUN_0_2:
      case ClosureTypes.FUN_STATIC: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i64Load(
              info +
                rtsConstants.offset_StgFunInfoTable_f +
                rtsConstants.offset_StgFunInfoExtraFwd_srt
            )
          );
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.BLACKHOLE:
      case ClosureTypes.MUT_VAR_CLEAN:
      case ClosureTypes.MUT_VAR_DIRTY:
      case ClosureTypes.PRIM:
      case ClosureTypes.MUT_PRIM:
      case ClosureTypes.COMPACT_NFDATA: {
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
        this.scavengePointersFirst(c + 8, ptrs);
        break;
      }
      case ClosureTypes.THUNK_STATIC:
      case ClosureTypes.THUNK:
      case ClosureTypes.THUNK_1_0:
      case ClosureTypes.THUNK_0_1:
      case ClosureTypes.THUNK_2_0:
      case ClosureTypes.THUNK_1_1:
      case ClosureTypes.THUNK_0_2: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i64Load(
              info + rtsConstants.offset_StgThunkInfoTable_srt
            )
          );
        const ptrs = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgThunk_payload,
          ptrs
        );
        break;
      }
      case ClosureTypes.THUNK_SELECTOR: {
        if (this.memory.i32Load(info + rtsConstants.offset_StgInfoTable_srt))
          this.evacuateClosure(
            this.memory.i64Load(
              info + rtsConstants.offset_StgThunkInfoTable_srt
            )
          );
        this.scavengeClosureAt(c + rtsConstants.offset_StgSelector_selectee);
        break;
      }
      case ClosureTypes.AP: {
        this.scavengePAP(
          c,
          rtsConstants.offset_StgAP_fun,
          c + rtsConstants.offset_StgAP_payload,
          this.memory.i32Load(c + rtsConstants.offset_StgAP_n_args)
        );
        break;
      }
      case ClosureTypes.PAP: {
        this.scavengePAP(
          c,
          rtsConstants.offset_StgPAP_fun,
          c + rtsConstants.offset_StgPAP_payload,
          this.memory.i32Load(c + rtsConstants.offset_StgPAP_n_args)
        );
        break;
      }
      case ClosureTypes.AP_STACK: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgAP_STACK_fun);
        this.scavengeStackChunk(
          c + rtsConstants.offset_StgAP_STACK_payload,
          c +
            rtsConstants.offset_StgAP_STACK_payload +
            Number(
              this.memory.i64Load(c + rtsConstants.offset_StgAP_STACK_size)
            )
        );
        break;
      }
      case ClosureTypes.IND_STATIC: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgIndStatic_indirectee);
        break;
      }
      case ClosureTypes.MVAR_CLEAN:
      case ClosureTypes.MVAR_DIRTY: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_head);
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_tail);
        this.scavengeClosureAt(c + rtsConstants.offset_StgMVar_value);
        break;
      }
      case ClosureTypes.ARR_WORDS: {
        break;
      }
      case ClosureTypes.MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.MUT_ARR_PTRS_FROZEN_CLEAN: {
        const ptrs = Number(
          this.memory.i64Load(c + rtsConstants.offset_StgMutArrPtrs_ptrs)
        );
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgMutArrPtrs_payload,
          ptrs
        );
        break;
      }
      case ClosureTypes.WEAK: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_cfinalizers);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_key);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_value);
        this.scavengeClosureAt(c + rtsConstants.offset_StgWeak_finalizer);
        break;
      }
      case ClosureTypes.TSO: {
        this.scavengeClosureAt(c + rtsConstants.offset_StgTSO_stackobj);
        break;
      }
      case ClosureTypes.STACK: {
        const stack_size = this.memory.i32Load(
            c + rtsConstants.offset_StgStack_stack_size
          ),
          sp = Number(this.memory.i64Load(c + rtsConstants.offset_StgStack_sp)),
          sp_lim = c + rtsConstants.offset_StgStack_stack + (stack_size << 3);
        this.scavengeStackChunk(sp, sp_lim);
        break;
      }
      case ClosureTypes.SMALL_MUT_ARR_PTRS_CLEAN:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
      case ClosureTypes.SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: {
        this.scavengePointersFirst(
          c + rtsConstants.offset_StgSmallMutArrPtrs_payload,
          Number(
            this.memory.i64Load(c + rtsConstants.offset_StgSmallMutArrPtrs_ptrs)
          )
        );
        break;
      }
      default:
        throw new WebAssembly.RuntimeError();
    }
  }

  /**
   * Allocates a new nursery and stores its address in the appropriate
   * field of the StgRegTable of the main capability.
   */
  updateNursery() {
    // Note: the 'rHpAlloc' field of the 'StgRegTable' C struct contains 
    // the number of bytes allocated in the heap, or better the number of
    // bytes attempted to being allocated before the heap check fails.
    // Here, we read this field in the hp_alloc variable and
    // use it to determine the size of the newly allocated nursery.
    const base_reg =
        this.symbolTable.MainCapability + rtsConstants.offset_Capability_r,
      hp_alloc = Number(
        this.memory.i64Load(base_reg + rtsConstants.offset_StgRegTable_rHpAlloc)
      );
    // reset the number of allocated bytes in the nursery
    this.memory.i64Store(
      base_reg + rtsConstants.offset_StgRegTable_rHpAlloc,
      0
    );
    // The address of the new nursery's block descriptor is stored
    // in the 'rCurrentNursery' field of the StgRegTable of the main capability.
    this.memory.i64Store(
      base_reg + rtsConstants.offset_StgRegTable_rCurrentNursery,
      this.heapAlloc.hpAlloc(hp_alloc)
    );
  }

  /**
   * Performs garbage collection, using scheduler Thread State Objects (TSOs) as roots.
   */
  performGC() {
    if (this.yolo || this.heapAlloc.liveSize() < this.gcThreshold) {
      // Garbage collection is skipped. This happens in yolo mode,
      // or when the total number of "live" MBlocks is below the given threshold
      // (by "live", we mean allocated and not yet freed - see HeapAlloc.liveSize).
      // This avoids a lot of GC invocations
      // (see {@link https://github.com/tweag/asterius/pull/379}).
      this.updateNursery();
      return;
    }
    this.reentrancyGuard.enter(1);
    this.heapAlloc.initUnpinned();

    // Evacuate TSOs
    for (const [_, tso_info] of this.scheduler.tsos) {
      tso_info.addr = this.evacuateClosure(tso_info.addr);
    }

    // Evacuate stable pointers
    for (const [sp, c] of this.stablePtrManager.spt.entries())
      if (!(sp & 1)) this.stablePtrManager.spt.set(sp, this.evacuateClosure(c));

    // Stage the movement of stable pointers.
    // Step 1: Move all the pointers
    // Step 2: Update the pointer -> stablepointer mapping
    // We cannot do this at the same time, since moving the pointer while
    // we walk the ptr2stable map can yield an infinite loop:
    // eg. (ptr:0 stablename: 42) --MOVE--> (ptr:1 stablename:42) --MOVE--> (ptr:2 stablename:42) ...
    let ptr2stableMoved = new Map();
    for (const [ptr, stable] of this.stableNameManager.ptr2stable.entries()) {
      const ptrMoved = this.evacuateClosure(ptr);
      const stableMoved = this.evacuateClosure(stable);
      ptr2stableMoved.set(ptrMoved, stableMoved);
    }
    this.stableNameManager.ptr2stable = ptr2stableMoved;

    // do the rest of the scavenging work
    this.scavengeWorkList();

    // update the ret pointer in the complete TSOs
    for (const [_, tso_info] of this.scheduler.tsos) {
      if (tso_info.ret) {
        const tso = tso_info.addr;
        const stackobj = Number(
          this.memory.i64Load(tso + rtsConstants.offset_StgTSO_stackobj)
        );
        const sp = Number(
          this.memory.i64Load(stackobj + rtsConstants.offset_StgStack_sp)
        );
        tso_info.ret = Number(this.memory.i64Load(sp + 8));
      }
    }

    // mark unused MBlocks
    this.heapAlloc.handleLiveness(this.liveMBlocks, this.deadMBlocks);
    // allocate a new nursery
    this.updateNursery();
    // garbage collect unused JSVals
    this.stablePtrManager.preserveJSVals(this.liveJSVals);
    // cleanup
    this.nonMovedObjects.clear();
    this.liveMBlocks.clear();
    this.deadMBlocks.clear();
    this.liveJSVals.clear();
    this.reentrancyGuard.exit(1);
  }
}
