'use babel';

//request autocomplete activate with ctrl-space

import { CompositeDisposable, Range } from 'atom';
import OSC from 'osc-js'

class AdvancedProvider {
	constructor() {
		this.subscriptions = new CompositeDisposable()
	    this.subscriptions.add(atom.commands.add('atom-text-editor', {
	      'myAutocomplete:get-highlights': () => this.getHighlights(),
	      'myAutocomplete:get-subseq': () => this.getSubSeq()
	    }))

		this.selector = '.source.tidalcycles'
		this.suggestionPriority = 2
		this.suggestions = []
		this.id = null
		this.oscComp = new OSC({
  	  plugin: new OSC.DatagramPlugin({
        open: {port: 8888},
        send: {port: 9999}
    	})
  	})
  	this.oscComp.on('open', () => console.log('opened HL'))
    this.oscComp.on('close', () => console.log('closed HL'))
    this.oscComp.on('error', (err) => console.log(err))
    this.oscComp.open()

		this.oscHL = new OSC({
  	  plugin: new OSC.DatagramPlugin({
        open: {port: 8889},
        send: {port: 9999}
    	})
  	})
  	this.oscHL.on('line_highlights', (msg) => this.parseHighlights(msg) )
  	this.oscHL.on('open', () => console.log('opened suggestion'))
    this.oscHL.on('close', () => console.log('closed suggestion'))
    this.oscHL.on('error', (err) => console.log(err))
  	this.oscHL.open()
	}

	getSuggestions(options) {
		const {editor, bufferPosition, activatedManually} = options

		let prefix = this.getPrefix(editor, bufferPosition)
		let selRange = editor.getLastSelection().getBufferRange()
		let reversed = editor.getLastSelection().isReversed()

		if (prefix.startsWith('$') || activatedManually === true) {
			return this.findMatchingSuggestions(prefix, editor, bufferPosition, selRange, reversed)
		}
	}

	getPrefix(editor, bufferPosition) {
		let line = editor.getTextInRange([[bufferPosition.row, 0], bufferPosition]);
		let match = line.match(/\S+$/)
		return match ? match[0] : ''
	}

	findMatchingSuggestions(prefix, editor, cursor, selection, reversed) {
		return new Promise((resolve, reject) => {
			this.id = this.oscComp.on('/reply', message => {
				this.parseOSC(message)
					.then((suggestions) => {
						this.oscComp.off('/reply', this.id);
						resolve(suggestions)
					})
			})
			if (selection.isEmpty()){
				this.oscComp.send(new OSC.Message('/subseq', prefix))
			} else {
    		let code = editor.getTextInBufferRange(selection)
				this.oscComp.send(new OSC.Message(
					'/subseq',
					JSON.stringify({
						'code':code,
						'range':selection.serialize(),
						'reversed':reversed
					}))
				)
			}
		})
	}

	async parseOSC(msg) {
		let msgArray = JSON.parse(msg.args[0])
		return msgArray.map(word => {
				let w = word.replace(/\'/g, "\"")
				return { text: w }
		})
	}

	getHighlights(){
		let editor = atom.workspace.getActiveTextEditor()
		var cursor = editor.getCursors()[0]
		var range = cursor.getCurrentParagraphBufferRange()
    	var code = editor.getTextInBufferRange(range)
		this.oscHL.send(new OSC.Message(
				'/subseq',
				code)
		)
	}

	parseHighlights(msg) {
		let editor = atom.workspace.getActiveTextEditor()
		let ranges = JSON.parse(msg.args[0])

		if (this.markerLayer === undefined)
    		this.markerLayer = editor.addMarkerLayer()
		this.markerLayer.clear()

		ranges.forEach(range => {
			this.markerLayer.markBufferRange(
				range,
				{invalidate: 'touch'}
			)
			editor.decorateMarkerLayer(
				this.markerLayer,
				{
					type: 'highlight',
					class: 'blue'
				})
		})
	}

	deactivate() {
		this.markerLayer.clear()
		this.oscComp.close()
		this.oscHL.close()
		this.subscriptions.dispose()
	}
}
export default new AdvancedProvider();
