# haskell-server
:rocket: Haskell rest api, postgresql + scotty

get /users
```json
[
  {
    "lastName": "spychala",
    "userId": 1,
    "team": 1,
    "firstName": "maciek"
  },
  {
    "lastName": "siemiawska",
    "userId": 2,
    "team": 2,
    "firstName": "magda"
  }
]
```
get /teams

```json
[
  {
    "teamId": 1,
    "name": "frontend"
  },
  {
    "teamId": 2,
    "name": "backend"
  }
]
```

get /tasks
```json
[
  {
    "exeqTeam": 2,
    "taskId": 2,
    "endDate": "2017-01-26T00:00:00Z",
    "beginDate": "2017-01-15T00:00:00Z",
    "description": "refactor code"
  },
  {
    "exeqTeam": 3,
    "taskId": 3,
    "endDate": "2017-01-22T00:00:00Z",
    "beginDate": "2017-01-16T00:00:00Z",
    "description": "prepare mockups"
  }
]
```

get /events
```json
[
  {
    "creator": 2,
    "eventName": "Srump details",
    "eventId": 2
  },
  {
    "creator": 3,
    "eventName": "New year",
    "eventId": 3
  }
]
```

get /checklists
```json
[
  {
    "checklistItems": [
      {
        "checklist": 2,
        "checklistItemId": 4,
        "finished": false,
        "itemText": "reformat code"
      },
      {
        "checklist": 2,
        "checklistItemId": 5,
        "finished": false,
        "itemText": "better methods naming"
      }
    ],
    "listOwner": 2,
    "checklistId": 2
  },
  {
    "checklistItems": [],
    "listOwner": 3,
    "checklistId": 3
  }
]
```
