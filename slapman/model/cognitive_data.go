package model

// TransData Holds Translation Data In / Out
type TransData struct {
	From string `json:"from"`
	To   string `json:"to"`
	Text string `json:"text"`
}
