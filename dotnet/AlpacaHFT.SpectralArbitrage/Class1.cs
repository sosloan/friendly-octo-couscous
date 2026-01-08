namespace AlpacaHFT.SpectralArbitrage;

public class ArbitrageStatistics
{
    private readonly object _sync = new();
    private int _processedUpdates;
    private int _detectedOpportunities;
    private double _totalScore;

    private double ComputeAverageUnsafe() =>
        _processedUpdates == 0 ? 0.0 : _totalScore / _processedUpdates;

    public int ProcessedUpdates
    {
        get
        {
            lock (_sync)
            {
                return _processedUpdates;
            }
        }
    }

    public int DetectedOpportunities
    {
        get
        {
            lock (_sync)
            {
                return _detectedOpportunities;
            }
        }
    }

    public double AverageScore
    {
        get
        {
            lock (_sync)
            {
                return ComputeAverageUnsafe();
            }
        }
    }

    public void RecordUpdate(double score, bool isOpportunity)
    {
        lock (_sync)
        {
            _processedUpdates++;
            if (isOpportunity)
            {
                _detectedOpportunities++;
            }

            _totalScore += score;
        }
    }

    public void Reset()
    {
        lock (_sync)
        {
            _processedUpdates = 0;
            _detectedOpportunities = 0;
            _totalScore = 0.0;
        }
    }

    public override string ToString()
    {
        lock (_sync)
        {
            return $"Processed: {_processedUpdates} | Opportunities: {_detectedOpportunities} | Avg Score: {ComputeAverageUnsafe():F4}";
        }
    }
}
