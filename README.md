bitgloom
========

Bitgloom is a sybil-resistant distributed bitcoin anonymization service that lives on top of the Bitcoin blockchain. It uses a cryptographically secure pairing mechanism first described by [Biassias et al. 2014](http://forensics.umass.edu/pubs/bissias.wpes.2014.pdf). In the paper they describe a mechanism where multiple peers can pair with each other to mix their bitcoins, and a secure mixing service can be built on top of this when one peer performs multiple rounds of mixing with different peers.

The implementation lives solely on top of the blockchain, and no other method of communication is required. 

Compared to other mixing technologies, Bitgloom provides the following benefits:

 * Complete anonimity, and no IP addresses are known: Bitgloom communicates entirely through the Blockchain and as such leverages the Bitcoin P2P network to hide in the mass;
 * A decentralized pairing protocol: unlike other protocols such as CoinShuffle, Bitgloom does not need peers to centralize their communications through a different P2P network;
 * There is no evidence of pairing through transactions: where other mixing protocols combine the mixing in a single transaction, there is no such evidence in Bitgloom; the only way to derive this evidence would be through a timing attack.
 * There is a cost involved in performing a mixing using Bitgloom, the transaction fees: this can actually be interpreted as an advantage, because this makes it unattractive (or even unviable) to perform a sybil (N-1) attack on Bitgloom.
