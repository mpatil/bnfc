{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Makefile.Simulator
  ( simulatorVariables
  , simulatorTargets
  ) where

import Prelude hiding ((<>))

import BNFC.Backend.Common.Makefile
import BNFC.Backend.SV.Config
import BNFC.PrettyPrint

simulatorVariables :: SVConfig -> [Doc]
simulatorVariables cfg =
  [ "QRUN = qrun"
  , "QRUN_OPTS = -64 -sv -mfcu -permissive -l comp.log -suppress 2875,2240 +incdir+./ +UVM_NO_RELNOTES"
  , "VCS = vcs"
  , "VCS_OPTS = -full64 -sverilog -l comp.log"
  , text $ "VCS_SIM = " ++ svFuseSoCRunDir cfg
  , "TOPLEVEL = test"
  , text $ "FUSESOC_RUN_DIR = " ++ svFuseSoCRunDir cfg
  , "TEST_VCS_DIR = ${FUSESOC_RUN_DIR}/test-vcs"
  , text $ "SIM_SCRIPT = " ++ svFuseSoCSimScript cfg
  , "TEST_INPUT = ../../test.w"
  ]

simulatorTargets :: SVConfig -> [Doc]
simulatorTargets _ =
  [ mkRule "run" ["fusesoc", "Test.sv"]
      [ "@if command -v ${QRUN} >/dev/null 2>&1; then \\"
      , "  ${MAKE} qrun; \\"
      , "elif command -v ${VCS} >/dev/null 2>&1; then \\"
      , "  ${MAKE} vcs; \\"
      , "else \\"
      , "  echo \"Neither ${QRUN} nor ${VCS} found on PATH\"; \\"
      , "  exit 127; \\"
      , "fi"
      ]
  , ".PHONY: qrun"
  , mkRule "qrun" ["fusesoc", "Test.sv"]
      [ "cd ${TEST_VCS_DIR} && ${QRUN} ${QRUN_OPTS} -F ${SIM_SCRIPT} ../../../Test.sv +input=${TEST_INPUT}" ]
  , ".PHONY: vcs"
  , mkRule "vcs" ["fusesoc", "Test.sv"]
      [ "cd ${TEST_VCS_DIR} && ${VCS} ${VCS_OPTS} +incdir+../.. -f ${SIM_SCRIPT} -top ${TOPLEVEL} -o ${VCS_SIM}"
      , "cd ${TEST_VCS_DIR} && ./${VCS_SIM} -l vcs.log +input=${TEST_INPUT}"
      ]
  ]
