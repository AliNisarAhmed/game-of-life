import './main.css';
import { Elm } from './src/Main.elm';
import oscillatorImage from './assets/oscillator.png';
import acorn from './assets/acorn.png';
import dieHard from './assets/diehard.png';
import glider from './assets/glider.png';
import gosper from './assets/gosper.png';
import rpentomino from './assets/rpentomino.png';
import talker from './assets/talker.png';
import toad from './assets/toad.png';

const initialWidth = 90;

Elm.Main.init({
	node: document.querySelector('main'),
	flags: {
		initialWidth,
		images: [
			{ name: 'Oscillator', file: oscillatorImage },
			{ name: 'Acorn', file: acorn },
			{ name: 'DieHard', file: dieHard },
			{ name: 'Glider', file: glider },
			{ name: 'Gosper Glider Gun', file: gosper },
			{ name: 'RPentomino', file: rpentomino },
			{ name: 'Talker', file: talker },
			{ name: 'Toad', file: toad },
		],
	},
});