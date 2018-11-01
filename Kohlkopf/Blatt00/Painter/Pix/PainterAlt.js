var counter = 0
var motion = true
var millisecs = 1000
lg = images.length
function start()
   {if (motion) {forth(); setTimeout("start()",millisecs)}}
function stopGo(button)
   {if (button.value == "stop") {motion = false; button.value = "go"} 
    else {motion = true; start(); button.value = "stop"}}
function back() 
   {if (counter > 0) {counter--} else {counter = lg-1}
    showFile(counter)}
function forth() 
   {if (counter < lg-1) {counter++} else {counter = 0}
    showFile(counter)}
function showTime(time)
   {document.getElementById("time").innerHTML = time; millisecs = time}
function showFile(i)
   {image = document.getElementById("img")
    slider = document.getElementById("fileSlider")
    document.getElementById("file").innerHTML = images[i]
    image.src = images[i] 
    slider.value = i
    counter = i}