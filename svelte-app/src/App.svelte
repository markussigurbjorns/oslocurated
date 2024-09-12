<script>
	export let name;
	let audioElement;
    
	function playStream() {
		// Refresh the stream by resetting the src, this forces a reload
		audioElement.src = "https://oslocurated.com/stream.mp3?" + new Date().getTime(); // Add a timestamp to force reload
		audioElement.play();
	}

	function pauseStream() {
		audioElement.pause();
	}
</script>

<main>
	<h1>Hello {name}!</h1>
	<p>Visit the <a href="https://svelte.dev/tutorial">Svelte tutorial</a> to learn how to build Svelte apps.</p>
	<div class="media-player">
		<audio bind:this={audioElement} preload="none"></audio>

		<div class="controls">
			<button on:click={playStream}>Play</button>
			<button on:click={pauseStream}>Pause</button>
		</div>
	</div>
</main>

<style>
	main {
		text-align: center;
		padding: 1em;
		max-width: 240px;
		margin: 0 auto;
	}

	h1 {
		color: #ff3e00;
		text-transform: uppercase;
		font-size: 4em;
		font-weight: 100;
	}

	@media (min-width: 640px) {
		main {
			max-width: none;
		}
	}
	.media-player {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		width: 100%;
	}

	.controls button {
		margin: 5px;
		padding: 10px 15px;
		background-color: #007bff;
		border: none;
		color: white;
		cursor: pointer;
		border-radius: 5px;
	}

	.controls button:hover {
		background-color: #0056b3;
	}
</style>
