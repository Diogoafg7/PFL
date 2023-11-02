% board(+SizeOfBase,+Matrix)
% Board structure
% Board Small
board(13, [
    [empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).

% Board Medium
board(17, [
    [empty, empty, empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).

% Board Large
board(21, [
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, notused, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty, empty],
    [empty, empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty, empty],
    [empty, empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty, empty],
    [empty, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, empty],
    [notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused, notused]
]).
