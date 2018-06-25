package js

import org.scalajs.dom
import org.scalajs.dom.html.{Button, Option, Select, TextArea}
import org.scalajs.dom.{Event, XMLHttpRequest}

import scala.scalajs.js.annotation.JSExportTopLevel

trait DemoJS extends util.Demo {
  var output: String = ""

  override def output(s: String): Unit = {
    output += s + "\n"
  }

  override def demo(s: String): Unit = {
    output = ""
    super.demo(s)
  }
}


object Demo {
  @JSExportTopLevel("js.Demo.main")
  def main(): Unit = {

    val demos: Array[DemoJS] = Array(
      ArithDemoJS,
      BotDemoJS,
      EquirecDemoJS,
      FullEquirecDemoJS,
      FullErrorDemoJS,
      FullFomSubDemoJS,
      FullFomSubRefDemoJS,
      FullFomSubDemoJS,
      FullIsorecDemoJS,
      FullOmegaDemoJS,
      FullPolyDemoJS,
      FullReconDemoJS,
      FullRefDemoJS,
      FullSimpleDemoJS,
      FullFomSubDemoJS,
      FullUntypedDemoJS,
      RcdSubBotDemoJS,
      ReconDemoJS,
      SimpleBoolDemoJS,
      TyArithDemoJS,
      UntypedDemoJS
    )

    val languageSelect = dom.document.getElementById("language").asInstanceOf[Select]
    val inputArea = dom.document.getElementById("input").asInstanceOf[TextArea]
    val outputArea = dom.document.getElementById("output")
    val runButton = dom.document.getElementById("run").asInstanceOf[Button]

    var z = 0
    for (demo <- demos) {
      z += 1
      val option = dom.document.createElement("option").asInstanceOf[Option]
      option.text = demo.name
      languageSelect.options(z) = option
    }

    languageSelect.onchange = loadProgram
    runButton.onclick = {_ => run()}

    def loadProgram(e: Event): Unit = {
      val index = languageSelect.selectedIndex
      val demo = demos(index - 1)
      loadProgramByPath(demo.defaultExample)
    }

    def loadProgramByPath(path: String): Unit = {
      val xhr = new XMLHttpRequest()
      xhr.onload = { _ =>
        if (xhr.status == 200) {
          inputArea.value = xhr.responseText
        } else {
          inputArea.value = xhr.status.toString
        }
      }
      xhr.open("GET", path, async = true)
      xhr.send()
    }

    def run(): Unit = {
      val index = languageSelect.selectedIndex
      if (index == 0) {
        dom.window.alert("Input language is not selected")
        return
      }
      outputArea.textContent = "Calculating..."
      val demo = demos(index - 1)
      val input = inputArea.value
      try {
        demo.demo(input)
      } catch {
        case e : Throwable =>
          // TODO
      } finally {
        outputArea.textContent = demo.output
      }
    }
  }
}
