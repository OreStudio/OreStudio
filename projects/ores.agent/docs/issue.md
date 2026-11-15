## Risk report fails because the generated ORE bundle is missing two XML files

The agent’s integration harness submits a risk report to a running OreStudio instance through the agent’s normal chat interface, polls it to completion, and checks the stored results. The test runs against a disposable database clone.

### Confirmed cause

In native run `30c86fb9-6d70-47bb-8e3e-455a10fb5592`, the generated `Input/ore.xml` referenced `simulation.xml` and `sensitivity.xml`, but the input archive contained neither file.

Replaying the captured archive directly with ORE confirmed the failure. The original failed opening `Input/simulation.xml`. Adding only that file moved the failure to `Input/sensitivity.xml`. Adding both files from the prepared test bundle let ORE complete and write `sensitivity.csv`.

### Proposed fix

Include both configuration files when OreStudio builds a bundle for an active sensitivity analytic. Validate the referenced files before launching ORE, and add a native integration regression test that verifies the report completes and its sensitivity results are stored.