package model

import (
	"bytes"
	"fmt"
	"time"

	"github.com/go-sql-driver/mysql"
)

func selectByPropQuery(tblName string, props ...string) string {
	var buffer bytes.Buffer
	buffer.WriteString(fmt.Sprintf("SELECT * FROM %s ct WHERE ", tblName))

	for i, prop := range props {
		if i == 0 {
			buffer.WriteString(fmt.Sprintf("ct.%s=? ", prop))
		} else {
			buffer.WriteString(fmt.Sprintf("AND ct.%s=? ", prop))
		}
	}

	return buffer.String()
}

func selectAllQuery(tblName string) string {
	return fmt.Sprintf("SELECT * FROM %s", tblName)
}

func selectRelatedQuery(tblName string, props ...string) string {
	var buffer bytes.Buffer
	buffer.WriteString(fmt.Sprintf("SELECT ctm.* FROM %s ctm WHERE ", tblName))

	for i, prop := range props {
		if i == 0 {
			buffer.WriteString(fmt.Sprintf("ctm.%s in (?) ", prop))
		} else {
			buffer.WriteString(fmt.Sprintf("AND ctm.%s in (?) ", prop))
		}
	}

	return buffer.String()
}

func countSelection(selectionQuery string, wrap bool) (query string) {
	if wrap {
		query = fmt.Sprintf("SELECT COUNT(*) FROM (%s) cs USE INDEX(PRIMARY)", selectionQuery)
	} else {
		query = fmt.Sprintf("SELECT COUNT(*) FROM %s cs USE INDEX(PRIMARY)", selectionQuery)
	}
	return
}

// http://www.iheavy.com/2013/06/19/3-ways-to-optimize-for-paging-in-mysql/
// http://www.xarg.org/2011/10/optimized-pagination-using-mysql/
// http://www.xarg.org/2011/10/optimized-pagination-using-mysql/
const (
	RowCount      = 20
	DefaultOffset = 0
)

// Transforms a Selection to Retrieve a Page (Un-Optimized for Now)
func pagedSelection(selectionQuery string, page int) string {
	var buffer bytes.Buffer

	if offset := (page - 1) * 20; offset > 0 {
		buffer.WriteString(fmt.Sprintf("%s LIMIT %d OFFSET %d", selectionQuery, RowCount, offset))
	} else {
		buffer.WriteString(fmt.Sprintf("%s LIMIT %d OFFSET %d", selectionQuery, DefaultOffset, DefaultOffset))
	}

	return buffer.String()
}

// AutoIncr Auto Increment
type AutoIncr struct {
	ID uint64 `db:"id" json:"id"`
	TimeStamps
}

// TimeAware Time Aware
type TimeAware interface {
	created() time.Time
	updated() time.Time
}

type byLastUpdated []TimeAware

func (blu byLastUpdated) Len() int {
	return len(blu)
}

func (blu byLastUpdated) Less(i, j int) bool {
	return blu[i].updated().After(blu[j].updated()) // Descending Order
	//	return blu[i].updated().Before(blu[j].updated()) // Ascending Order
}

func (blu byLastUpdated) Swap(i, j int) {
	blu[i], blu[j] = blu[j], blu[i]
}

//https://github.com/go-sql-driver/mysql#timetime-support

// TimeStamps Time Stamps
type TimeStamps struct {
	Created mysql.NullTime `db:"created" json:"created"`
	Updated mysql.NullTime `db:"updated" json:"updated"`
}

func (ts *TimeStamps) created() time.Time {
	return ts.Created.Time
}

func (ts *TimeStamps) updated() time.Time {
	return ts.Updated.Time
}

// Role Role
type Role struct {
	Name        string
	Default     int
	Permissions int
	AutoIncr
}
