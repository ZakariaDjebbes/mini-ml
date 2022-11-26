namespace ml_web.Models;

public class HomeViewModel
{
    public bool UseLib { get; set; }
    public string? Type { get; set; }
    public string? Reduced { get; set; }
    public string? AlphaConverted { get; set; }
    public string? Term { get; set; }
    public bool Error { get; set; }
    public string? ErrorText { get; set; }
}