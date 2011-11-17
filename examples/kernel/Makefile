PBC=../../src/pbc

.PHONY: all clean test testVA testMetric testMetricVA

all: nwkde nwkdeVA nwkdeMetric

clean:
	rm -f nwkde nwkdeVA nwkdeMetric

nwkde: nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

nwkdeVA: nwkdeVA.pbcc nwkde.pbcc nwkdeGenerators.pbcc nwkdeMetric.pbcc utils.pbcc nwkde.h
	${PBC} $< --output $@

nwkdeMetric: nwkdeMetric.pbcc nwkde.h
	${PBC} $< --output $@

KernelEstimate: KernelEstimate.pbcc LinearSolvePosDef.pbcc
	${PBC} $< --output $@

BinnedLR: BinnedLR.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -

testVA: nwkdeVA
	./nwkdeVA test/trainData.pba test/wrapFlags.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -

testMetric: nwkdeMetric nwkde
	./nwkde test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba testMetric.out.pba
	./nwkdeMetric testMetric.out.pba test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -
	rm testMetric.out.pba

testMetricVA: nwkdeMetric nwkdeVA
	./nwkdeVA test/trainData.pba test/wrapFlags.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba testMetricVA.out.pba
	./nwkdeMetric testMetricVA.out.pba test/trainData.pba test/wrapFlags.pba test/kernelWidths.pba test/trainIndices.pba test/trainVarIndex.pba test/trainTimeOffset.pba test/testData.pba test/testIndices.pba test/testVarIndex.pba test/testTimeOffset.pba test/outputVarIndex.pba test/outputTimeOffset.pba test/trainMaskWidth.pba -
	rm testMetricVA.out.pba

testBinnedLR: BinnedLR
	./BinnedLR btest/trainX.pba btest/trainDirs.pba btest/trainY.pba btest/indices.pba btest/testX.pba btest/testDirs.pba btest/indices.pba ../../testdata/Zero0D ../../testdata/Zero0D btest/output.pba
	./BinnedLR btest/trainX.pba btest/trainDirs.pba btest/trainY.pba btest/indices.pba btest/trainX.pba btest/trainDirs.pba btest/indices.pba ../../testdata/Eleven0D ../../testdata/Twelve0D btest/output2.pba
