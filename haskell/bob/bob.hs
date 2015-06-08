module Bob (responseFor) where

responseFor :: String -> String
responseFor message
	|isNothing message = "Fine. Be that way!"
	|isYelling message = "Whoa, chill out!"
	|isQuestion message = "Sure."
	|otherwise = "Whatever."
	where
		isNothing :: String -> Bool
		isNothing message = length [y | y<-message, not $ y `elem` whitespace] == 0
			where
				whitespace = [' ', '\t', '\n', '\r', '\v', '\xA0', '\x2002']

		isYelling :: String -> Bool
		isYelling message = (length [y | y<-message, y `elem` lowerCase] == 0) && (length [y | y<-message, y `elem` upperCase] > 0)
			where 
				lowerCase = '\xe4':['a'..'z']
				upperCase = ['A'..'Z'] ++ ['\xc4', '\xdc']

		isQuestion :: String -> Bool
		isQuestion message = last message == '?'