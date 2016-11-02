%%%-------------------------------------------------------------------
%%% @author yj
%%% @doc
%%%
%%% Created : 28. 七月 2016 上午9:52
%%%-------------------------------------------------------------------

-define(LOG_ATTR_ITEM(TabNameBin, PlayerId, Type, TypeId, Num, Now),
    case Type of
        add ->
            <<"INSERT INTO `", TabNameBin/binary, "` (`player_id`, `type_id`, `v`, `times`) VALUES ('",
                (integer_to_binary(PlayerId))/binary, "', '",
                (integer_to_binary(TypeId))/binary, "', '",
                Num/binary, "', ",
                Now/binary, ");">>;
        del ->
            <<"INSERT INTO `", TabNameBin/binary, "` (`player_id`, `type_id`, `v`, `times`) VALUES ('",
                (integer_to_binary(PlayerId))/binary, "', '",
                (integer_to_binary(TypeId))/binary, "', '-",
                Num/binary, "',",
                Now/binary, ");">>
    end).
