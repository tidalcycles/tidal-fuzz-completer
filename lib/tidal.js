'use babel';

import OSC from 'osc-js'
import {CompositeDisposable} from 'atom'

var CONST_LINE = 'line'
var CONST_MULTI_LINE = 'multi_line'

export default class Tidal {

  constructor(package) {
    this.package = package
    this.editor = atom.workspace.getActiveTextEditor()
    this.rmsToggle = 0
    this.markers = []
    this.subscriptions = new CompositeDisposable()
    this.subscriptions.add(atom.commands.add('atom-workspace', {
      'myAutocomplete:toggle-rms': () => this.rms(),
      'myAutocomplete:eval': () => this.eval('line'),
      'myAutocomplete:eval-multi-line': () => this.eval('multi_line')
    }))
    this.start()
    this.uvMarker(1)
  }

  start() {
    this.oscSC = new OSC({
        plugin: new OSC.DatagramPlugin({
            send: {port: 57110}
        })
    })
    this.oscSC.on('/rms', message => {
      var line = document.getElementsByClassName('gain')
      line.item(0).style.background =
        'linear-gradient(to right, rgba(255,0,0,'+(message.args[3]*10)+') 50%, rgba(255,0,0,'+(message.args[5]*10)+') 50%)'
    })
    this.oscSC.on('open', () => console.log('opened SC'))
    this.oscSC.on('close', () => console.log('closed SC'))
    this.oscSC.on('error', (err) => console.log(err))
    this.oscSC.open()

    this.oscTidal = new OSC({
        plugin: new OSC.DatagramPlugin({
          open: {port: 6012},
          send: {port: 6011}
        })
    })
    this.oscTidal.on('/pong', () => console.log("pong"))
    this.oscTidal.on('/code/ok', () => console.log("ok"))
    this.oscTidal.on('/name/ok', message => console.log(message))
    this.oscTidal.on('/expand/ok', message => console.log(message))
    this.oscTidal.on('/port/ok', message => console.log(message))
    this.oscTidal.on('/cps/set', message => console.log(message))
    this.oscTidal.on('/highlights/*', message => console.log(message))

    this.oscTidal.on('/code/highlight', message => {
        //array of (id, x, y, length)
    })

    //msg[1] = true/false -- add highlighting to variables currently active?
    this.oscTidal.on('/nowplaying', message => console.log(message))
    this.oscTidal.on('open', () => console.log('opened tidal'))
    this.oscTidal.on('close', () => console.log('closed tidal'))
    this.oscTidal.on('error', (err) => console.log(err))
    this.oscTidal.open()
  }

  eval(evalType) {
    var self = this
    var expressionAndRange = this.currentExpression(evalType, this.editor)
    this.oscTidal.send(new OSC.Message('/code', "1", expressionAndRange[0]))
    if (expressionAndRange[1]) {
      var range = expressionAndRange[1]
      function doIt() {
        var unflash
        if (range) {
          unflash = self.evalFlash(range, self.editor)
        }

        function onSuccess() {
          if (unflash) unflash('eval-success')
        }

        onSuccess()
      }
      doIt()
    }
  }

  rms() {
    this.rmsToggle = (this.rmsToggle + 1) % 2
    this.oscSC.send(new OSC.Message('/notify', this.rmsToggle))
  }

  sendControl(address, message){
    //port 6010
    // this.oscCtlClient.send('/'+address, message)
  }

  currentExpression(evalType, editor) {
    if (!editor) return
    var cursor = editor.getCursors()[0]
    var range = null
    if (evalType === CONST_LINE) {
      range = cursor.getCurrentLineBufferRange()
    } else {
      range = cursor.getCurrentParagraphBufferRange()
    }
    var expression = editor.getTextInBufferRange(range)
    return [expression, range]
  }

  evalFlash(range, editor) {
    var marker = editor.markBufferRange(range, {
      invalidate: 'touch'
    })
    var decoration = editor.decorateMarker(
      marker, {
        type: 'line',
        class: 'eval-success'
      })

    return function(cssClass) {
      decoration.setProperties({
        type: 'line',
        class: cssClass
      })
      var destroy = function() {
        marker.destroy()
      }
      setTimeout(destroy, 120)
    }
  }

  uvMarker(where){
    // this.removeDecorations()
    var marker = this.editor.markBufferRange(
      [[where-1, 0],[where, 0]],
      {invalidate: 'never'});

    var decoration = this.editor.decorateMarker(
      marker, {
        type: 'line-number',
        class: 'gain'
      });
    this.markers.push(marker)
  }

  removeDecorations () {
    for (var i = 0; i < this.markers.length; i++) {
      this.markers[i].destroy()
    }
  }

  markRange (startRow, endRow, klass) {
    const marker = this.editor.markBufferRange([[startRow, 0], [endRow, 0]], {invalidate: 'never'})
    this.editor.decorateMarker(marker, {type: 'line-number', class: klass})
    this.markers.push(marker)
  }

  destroy() {
    this.oscSC.close()
    this.oscTidal.close()
    this.removeDecorations()
    this.subscriptions.dispose()
  }

}
