
# pokerbots
This should be a poker simulation where bots can compete against each other. The function to create a custom bot is
```sh
pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
```
where the first argument is the name of the bot, the RoundStartAction represents the bots initial strategy when the poker round is started and PlayAction represents the strategy played during subsequent player turns in that round. For example
a bot that bets 20 and then always calls is

```sh
pokerBot "Daredevil" (return (Bet 20)) (return Call)
```

Each bot has access to the following data:
```sh
data BotState = BotState {_hole :: Hand, _moneyLeft :: Int, _investedInPot :: Int, _callNeeded :: Int, _potTotal :: [PotSimple], _communityCards :: CommunityCards}
```
where *hole* represents the cards in hand, *moneyLeft* is the available money that the bot currently owns, *investedInPot* is tha amount that the bot invested into the pot, *potTotal* is the total amount of money in the pot and *communityCards* are the cards on the table.     

A PlayAction is defined as 
```sh
data PlayAction = Fold | Call | Raise Money
```

and RoundStartAction is
```sh
data RoundStartAction = Check | Bet Money | Fold_
```

### Status
Although the game mechanics works (card evaluation, bots, drawing cards) the actual game was not yet implemented.
There are several poker games with subtle differences. The first poker game to be implemented is Texas Holdem Poker. 
This currently is not finished. This project is **just an experiment**.
