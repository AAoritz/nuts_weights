status <- system('rsvg-convert -z 6 ./figs/workflow.svg -o ./figs/workflow.png')
if (status != 0) {
  warning("rsvg-convert failed (status ", status, "); figs/workflow.png was not regenerated.")
}
