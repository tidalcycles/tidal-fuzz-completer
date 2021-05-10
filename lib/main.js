'use babel';

import Tidal from './tidal'
import advancedProvider from './advanced-provider'
import {CompositeDisposable} from 'atom'

export default {
  tidal: null,

  config: {
    // 'TidalInPort': {
    //   type: 'integer',
    //   default: 6012,
    //   description: 'Tidal In Port'
    // }
  },

  activate(state) {
      this.subscriptions = new CompositeDisposable()
      this.subscriptions.add(atom.commands.add('atom-workspace', {
        'myAutocomplete:start-tidal-cycles': () => this.startTidalCycles(state),
        'myAutocomplete:stop-tidal-cycles': () => this.stopTidalCycles()
      }))
  },

  startTidalCycles(state) {
    this.tidal = new Tidal(this)
  },

  stopTidalCycles() {
    if (this.tidal !== null) {this.tidal.destroy()}
  },

  getProvider() {
    return [advancedProvider]
  }

};
