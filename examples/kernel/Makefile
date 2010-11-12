PBC=../../src/pbc

all: nwkde nwkdelm

nwkde: nwkde.pbcc utils.pbcc
	${PBC} $< --output $@

nwkdelm: nwkdelm.pbcc utils.pbcc
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

test1: nwkde
	./nwkde test1/trainData.pba test1/wrapFlags.pba test1/kernelWidths.pba test1/trainIndices.pba test1/trainVarIndex.pba test1/trainTimeOffset.pba test1/testData.pba test1/trainIndices.pba test1/testVarIndex.pba test1/testTimeOffset.pba test1/outputVarIndex.pba test1/outputTimeOffset.pba -

test2: nwkde
	./nwkde test2/trainData.pba test2/wrapFlags.pba test2/kernelWidths.pba test2/trainIndices.pba test2/trainVarIndex.pba test2/trainTimeOffset.pba test2/testData.pba test2/trainIndices.pba test2/testVarIndex.pba test2/testTimeOffset.pba test2/outputVarIndex.pba test2/outputTimeOffset.pba -

test1lm: nwkdelm
	./nwkdelm test1/trainData.pba test1/wrapFlags.pba test1/kernelWidths.pba test1/trainIndices.pba test1/trainVarIndex.pba test1/trainTimeOffset.pba test1/testData.pba test1/trainIndices.pba test1/testVarIndex.pba test1/testTimeOffset.pba test1/outputVarIndex.pba test1/outputTimeOffset.pba -

test2lm: nwkdelm
	./nwkdelm test2/trainData.pba test2/wrapFlags.pba test2/kernelWidths.pba test2/trainIndices.pba test2/trainVarIndex.pba test2/trainTimeOffset.pba test2/testData.pba test2/trainIndices.pba test2/testVarIndex.pba test2/testTimeOffset.pba test2/outputVarIndex.pba test2/outputTimeOffset.pba -
