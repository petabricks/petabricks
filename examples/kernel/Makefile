PBC=../../src/pbc

all: nwkde nwkdelm2

nwkde: nwkde.pbcc utils.pbcc
	${PBC} $< --output $@

nwkdelm2: nwkdelm2.pbcc utils.pbcc
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba -

testlm2: nwkdelm2
	./nwkdelm2 test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba -

