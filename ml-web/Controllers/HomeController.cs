using System.Diagnostics;
using Core;
using Microsoft.AspNetCore.Mvc;
using ml_web.Models;
using Type = Core.Type;

namespace ml_web.Controllers;

public class HomeController : Controller
{
    private readonly ILogger<HomeController> _logger;

    public HomeController(ILogger<HomeController> logger)
    {
        _logger = logger;
    }

    public IActionResult Index()
    {
        var res = CoreProgram.infer_and_eval("let x = 1 in x + 1", false);

        return View(new HomeViewModel
        {
            Term = Term.string_of_term(res.Item1),
            AlphaConverted = Term.string_of_term(res.Item2),
            Type = Type.string_of_type(res.Item3),
            Reduced = Term.string_of_term(res.Item4),
            Error = false,
            ErrorText = ""
        });
    }

    [HttpPost]
    public ActionResult Index(HomeViewModel model)
    {
        try
        {
            var res = CoreProgram.infer_and_eval(model.Term, model.UseLib);
            
            return View(new HomeViewModel
            {
                Term = Term.string_of_term(res.Item1),
                AlphaConverted = Term.string_of_term(res.Item2),
                Type = Type.string_of_type(res.Item3),
                Reduced = Term.string_of_term(res.Item4),
                Error = false,
                ErrorText = ""
            });
        }
        catch (Exception e)
        {
            return View(new HomeViewModel
            {
                Term = model.Term,
                Error = true,
                ErrorText = e.Message
            });
        }
    }
    
    [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
    public IActionResult Error()
    {
        return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
    }
}