module Main

import Control.App
import Control.App.Console
import Control.App.FileIO
import Data.List
import System
import System.File
import FinScLens

%default total

public export
runPureAuditSuite : Has [Console] e => App e (Bool, List String)
runPureAuditSuite = do
  let banner = [ "================================================================"
               , "  FINSC-LENS: Non-Perturbing Observer Horizons & Readout Audits "
               , "  (FinSc-Box -> Finite-Science Blind Dynamics -> FinSc-Lens)    "
               , "================================================================" ]
  traverse_ putStrLn banner
  (p1, l1)     <- runAudit1
  (p2, l2)     <- runAudit2
  (p3, l3)     <- runAudit3
  (p4, l4, cA) <- runAudit4and5
  (p6, l6)     <- runAudit6 cA
  let ok = p1 && p2 && p3 && p4 && p6
  let footer = [ "================================================================"
               , if ok
                   then "ALL 6 FINSC-LENS AUDITS PASSED (Blind Dynamics + External Lenses + TopologicalRelic)"
                   else "FINSC-LENS AUDITS FAILED"
               ]
  traverse_ putStrLn footer
  pure (ok, banner ++ l1 ++ l2 ++ l3 ++ l4 ++ l6 ++ footer)

public export
covering
writeTelemetryFile : FileIO e => String -> String -> App e Bool
writeTelemetryFile path body =
  withFile path WriteTruncate
    (\_ => pure False)
    (\h => do fPutStrLn h body
              fflush h
              pure True)

public export
covering
runWithTelemetry : HasErr AppHasIO e => App e Bool
runWithTelemetry = do
  (ok, lines) <- runPureAuditSuite
  wrote <- withFileIO
    (writeTelemetryFile "audit_trace.txt" (fastConcat (intersperse "\n" lines)))
    (\res => pure res)
    (\err => do putStrLn ("FileIO telemetry error: " ++ show err)
                pure False)
  putStrLn ("Boundary telemetry written to audit_trace.txt: " ++ show wrote)
  pure (ok && wrote)

public export
covering
main : IO ()
main = do
  ok <- run runWithTelemetry
  if ok then pure () else exitFailure
