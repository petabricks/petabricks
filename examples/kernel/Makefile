PBC=../../src/pbc -jobs 4

.PHONY: all clean test testVA testMetric testMetricVA testBinnedLR testEstimate

all: nwkde nwkdeVA nwkdeMetric BinnedLR

clean:
	rm -f nwkde nwkdeVA nwkdeMetric BinnedLR

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

Estimate: Estimate.pbcc
	${PBC} $< --output $@

test: nwkde
	./nwkde test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba test/output.pba.last
	diff test/output.pba test/output.pba.last

testVA: nwkdeVA
	./nwkdeVA test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/split.pba test/maskWidth.pba -

testMetric: nwkdeMetric nwkde
	./nwkde test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba test/output.pba.last
	./nwkdeMetric test/output.pba.last test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba -

testMetricVA: nwkdeMetric nwkdeVA
	./nwkdeVA test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/split.pba test/maskWidth.pba testMetricVA.out.pba
	./nwkdeMetric testMetricVA.out.pba test/trainData.pba test/trainX.pba test/trainY.pba test/trainIndices.pba test/testData.pba test/testX.pba test/testIndices.pba test/dirFlags.pba test/kernelWidths.pba test/split.pba test/maskWidth.pba -
	rm testMetricVA.out.pba

testBinnedLR: BinnedLR
	./BinnedLR btest/trainData.pba btest/x.pba btest/dir.pba btest/y.pba btest/indices.pba btest/testData.pba btest/x.pba btest/dir.pba btest/indices.pba ../../testdata/Zero0D ../../testdata/Zero0D btest/output.pba.last
	./BinnedLR btest/trainData.pba btest/x.pba btest/dir.pba btest/y.pba btest/indices.pba btest/testData.pba btest/x.pba btest/dir.pba btest/indices.pba ../../testdata/Ten0D ../../testdata/Twelve0D btest/output2.pba.last
	diff btest/output.pba btest/output.pba.last
	diff btest/output2.pba btest/output2.pba.last

testEstimate: Estimate
	./Estimate etest/data.pba etest/x.pba etest/dir.pba etest/y.pba etest/indices.pba etest/dirFlags.pba etest/split.pba etest/maskWidth.pba etest/output.pba.last
	diff etest/output.pba etest/output.pba.last

testEstimateMetric: EstimateMetric
	./Estimate etest/data.pba etest/x.pba etest/dir.pba etest/y.pba etest/indices.pba etest/dirFlags.pba etest/split.pba etest/maskWidth.pba etest/output.pba.last
	cat etest/output.pba.last | sed s/nan/-667/g > etest/output.pba.last.2
	mv etest/output.pba.last.2 etest/output.pba.last
	./EstimateMetric etest/output.pba.last etest/data.pba etest/x.pba etest/dir.pba etest/y.pba etest/indices.pba etest/dirFlags.pba etest/split.pba etest/maskWidth.pba -
