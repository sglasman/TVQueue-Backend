import qualified InboundControllerQCTest as Tests1
import qualified IntegrationScenario as Tests2

main :: IO ()
main = Tests1.runAll >> Tests2.runAll