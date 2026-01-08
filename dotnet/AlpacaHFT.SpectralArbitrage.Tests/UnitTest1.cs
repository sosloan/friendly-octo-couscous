using AlpacaHFT.SpectralArbitrage;
using Xunit;

namespace AlpacaHFT.SpectralArbitrage.Tests;

public class ArbitrageStatisticsTests
{
    [Fact]
    public void InitialState_HasZeroValues()
    {
        // Arrange & Act
        var stats = new ArbitrageStatistics();
        
        // Assert
        Assert.Equal(0, stats.ProcessedUpdates);
        Assert.Equal(0, stats.DetectedOpportunities);
        Assert.Equal(0.0, stats.AverageScore);
    }

    [Fact]
    public void RecordUpdate_IncreasesProcessedCount()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        
        // Act
        stats.RecordUpdate(0.5, false);
        stats.RecordUpdate(0.7, true);
        
        // Assert
        Assert.Equal(2, stats.ProcessedUpdates);
    }

    [Fact]
    public void RecordUpdate_WithOpportunity_IncreasesOpportunityCount()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        
        // Act
        stats.RecordUpdate(0.5, false);
        stats.RecordUpdate(0.7, true);
        stats.RecordUpdate(0.6, true);
        
        // Assert
        Assert.Equal(2, stats.DetectedOpportunities);
    }

    [Fact]
    public void AverageScore_CalculatesCorrectly()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        
        // Act
        stats.RecordUpdate(0.4, false);
        stats.RecordUpdate(0.6, false);
        stats.RecordUpdate(0.8, true);
        
        // Assert - (0.4 + 0.6 + 0.8) / 3 = 0.6
        Assert.Equal(0.6, stats.AverageScore, precision: 6);
    }

    [Fact]
    public void Reset_ClearsAllValues()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        stats.RecordUpdate(0.5, true);
        stats.RecordUpdate(0.7, true);
        
        // Act
        stats.Reset();
        
        // Assert
        Assert.Equal(0, stats.ProcessedUpdates);
        Assert.Equal(0, stats.DetectedOpportunities);
        Assert.Equal(0.0, stats.AverageScore);
    }

    [Fact]
    public void ToString_ReturnsFormattedString()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        stats.RecordUpdate(0.5, true);
        
        // Act
        var result = stats.ToString();
        
        // Assert
        Assert.Contains("Processed: 1", result);
        Assert.Contains("Opportunities: 1", result);
        Assert.Contains("Avg Score:", result);
    }

    [Fact]
    public async Task ThreadSafety_ConcurrentUpdates()
    {
        // Arrange
        var stats = new ArbitrageStatistics();
        var tasks = new Task[10];
        
        // Act - multiple concurrent updates
        for (int i = 0; i < 10; i++)
        {
            tasks[i] = Task.Run(() =>
            {
                for (int j = 0; j < 100; j++)
                {
                    stats.RecordUpdate(0.5, j % 2 == 0);
                }
            });
        }
        
        await Task.WhenAll(tasks);
        
        // Assert
        Assert.Equal(1000, stats.ProcessedUpdates);
        Assert.Equal(500, stats.DetectedOpportunities);
    }
}
