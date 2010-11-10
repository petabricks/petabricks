PBC=../../src/pbc

nwkde: nwkde.pbcc utils.pbcc
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test1/trainData.pba test1/wrapFlags.pba test1/kernelWidths.pba test1/trainIndices.pba test1/trainVarIndex.pba test1/trainTimeOffset.pba test1/testData.pba test1/trainIndices.pba test1/testVarIndex.pba test1/testTimeOffset.pba test1/outputVarIndex.pba test1/outputTimeOffset.pba -
