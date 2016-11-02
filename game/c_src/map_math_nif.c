#include "erl_nif.h"
#include "math.h"


// 编译方式: nif目录下
// gcc -fPIC -shared -o priv/map_math_nif.so c_src/map_math_nif.c -I /usr/local/lib/erlang/usr/include/ -std=c99


// nif 说明文档:
// http://erlang.org/doc/man/erl_nif.html

// 具体使用方法:
// 在同名erl文件下调用:
// erlang:load_nif("./map_math", 0).

// 具体到这个文件的加载方式是map_math:init().
// 加载成功后 map_math:get_in_chunks(1.0,2.0,3.0,4) -> [{-1,1},{1,2},{1,1}]

// NIF很危险,一旦出错整个节点奔溃,所以NIF一定要测仔细!!
// 主要是传入参数类型绝对不能错,比如c里定义的 double 一定要传"1.0"这样的, 传"1"就崩!!反之亦然!!

//错误1:{error,{bad_lib,"Library module name 'map_math' does not match calling module 'erl_eval'"}}
//不能和hipe一起使用



//// 实际调用函数,测试用
//static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
//{
//    // 获得参数,注意,参数类型一定不能弄错
//	double A1;
//	int A2;
//
//	enif_get_double(env, argv[0], &A1);
//	enif_get_int(env, argv[1], &A2);
//
//	return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
//}

#define M_PI 3.14159265358979323846

static int get_chunk_1(double Num1, int ChunkWeight)
{
    int Num = (int)(round(Num1));
    if(Num > 0){
        return Num / ChunkWeight;
    }
	else{
		return Num / ChunkWeight - 1;
	}
}


// 点在圆中
static int in_c_handler(int X1, int Y1, int R)
{
	return X1*X1+ Y1*Y1 <= R * R;
}


// 根据圆获取所在块
static ERL_NIF_TERM get_in_chunks(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	// 获得参数,注意,参数类型一定不能弄错
	double X, Y;
	int R, ChunkWeight, MaxVertexX, MaxVertexY, MinVertexX, MinVertexY;
	enif_get_double(env, argv[0], &X);
	enif_get_double(env, argv[1], &Y);
	enif_get_int(env, argv[2], &R);
	enif_get_int(env, argv[3], &ChunkWeight);


	int ChunkX = get_chunk_1(X, ChunkWeight);
	int ChunkY = get_chunk_1(Y, ChunkWeight);


	MaxVertexX = (ChunkX + 1) * ChunkWeight;
	MaxVertexY = (ChunkY + 1) * ChunkWeight;
	MinVertexX = MaxVertexX - ChunkWeight;
	MinVertexY = MaxVertexY - ChunkWeight;

    ERL_NIF_TERM IntX = enif_make_int(env, ChunkX);
    ERL_NIF_TERM IntY = enif_make_int(env, ChunkY);
	ERL_NIF_TERM res = enif_make_list(env, 1, enif_make_tuple2(env, IntX, IntY));

	if((X + R) > MaxVertexX){
	    if((Y + R) > MaxVertexY){
	        res = enif_make_list_cell(env,
                enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY+1)),
                res);
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,enif_make_int(env, enif_make_int(env, ChunkX+1)),enif_make_int(env, enif_make_int(env, ChunkY+1))),
                res);
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,enif_make_int(env, ChunkX+1),IntY),
                res);
	    }else if((Y - R) < MinVertexY){
	        res = enif_make_list_cell(env,
                enif_make_tuple2(env,enif_make_int(env, ChunkX+1),IntY),
                res);
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,enif_make_int(env, ChunkX+1),enif_make_int(env, ChunkY-1)),
                res);
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY-1)),
                res);
	    }
	    else{
	        res = enif_make_list_cell(env,
                enif_make_tuple2(env,enif_make_int(env, ChunkX+1),IntY),
                res);
	    }
    }else if((X - R) < MinVertexX){
        if((Y - R) < MinVertexY){
            res = enif_make_list_cell(env,
                    enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY-1)),
                    res);
            res = enif_make_list_cell(env,
                    enif_make_tuple2(env,enif_make_int(env, ChunkX-1),enif_make_int(env, ChunkY-1)),
                    res);
            res = enif_make_list_cell(env,
                    enif_make_tuple2(env,enif_make_int(env, ChunkX-1),IntY),
                    res);
        }else if((Y + R) > MaxVertexY){
            res = enif_make_list_cell(env,
                    enif_make_tuple2(env,enif_make_int(env, ChunkX-1),IntY),
                    res);
            res = enif_make_list_cell(env,
                    enif_make_tuple2(env,enif_make_int(env, ChunkX-1),enif_make_int(env, ChunkY+1)),
                    res);
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY+1)),
                res);
        }else{
            res = enif_make_list_cell(env,
                enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY-1)),
                res);
        }
    }else if((Y + R) > MaxVertexY){
        res = enif_make_list_cell(env,
            enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY+1)),
            res);
    }else if((Y - R) < MinVertexY){
        res = enif_make_list_cell(env,
        enif_make_tuple2(env,IntX,enif_make_int(env, ChunkY-1)),
        res);
    }
    else{
        return res;
    }
    return res;

}


// 获取一个坐标点所处的块坐标
static ERL_NIF_TERM get_xyz(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	double X1, Y1;
	int ChunkWeight, ChunkX, ChunkY;
	enif_get_double(env, argv[0], &X1);
    enif_get_double(env, argv[1], &Y1);
    enif_get_int(env, argv[2], &ChunkWeight);


    int X2 = (int)(round(X1));
    if(X2 > 0){
        ChunkX = X2 / ChunkWeight;
    }else{
        ChunkX = X2 / ChunkWeight - 1;
    }
    int Y2 = (int)(round(Y1));
    if(Y2 > 0){
        ChunkY = Y2 / ChunkWeight;
    }else{
        ChunkY = Y2 / ChunkWeight - 1;
    }
    return enif_make_tuple2(env, enif_make_int(env, ChunkX), enif_make_int(env, ChunkY));
}


// 最多的块，只要有点在地图内
static ERL_NIF_TERM map_chunks(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int MapR, ChunkWeight, MaxChunkX, MaxChunkY;
	enif_get_int(env, argv[0], &MapR);
    enif_get_int(env, argv[1], &ChunkWeight);

    MaxChunkX = MapR / ChunkWeight;
    MaxChunkY = MaxChunkX;
    ERL_NIF_TERM res = enif_make_list(env, 0);
    for(int m = -MaxChunkX; m < MaxChunkX; m ++ ){
        for(int n = -MaxChunkY; n < MaxChunkY; n ++ ){
            if(in_c_handler(m * ChunkWeight, n * ChunkWeight, MapR)){
                res = enif_make_list_cell(env, enif_make_tuple2(env, enif_make_int(env, m), enif_make_int(env, n)), res);
            }
        }
    }
    return res;
}


// 获取视野
static ERL_NIF_TERM view_chunks(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	double X1, Y1;
	int MapR, ChunkWeight;
	enif_get_double(env, argv[0], &X1);
    enif_get_double(env, argv[1], &Y1);
    enif_get_int(env, argv[2], &ChunkWeight);
    enif_get_int(env, argv[3], &MapR);

    int Chunks[9][2];
    int ChunkX = get_chunk_1(X1, ChunkWeight);
    int ChunkY = get_chunk_1(Y1, ChunkWeight);

    Chunks[0][0] = ChunkX;
    Chunks[0][1] = ChunkY;

    Chunks[1][0] = ChunkX;
    Chunks[1][1] = ChunkY+1;

    Chunks[2][0] = ChunkX+1;
    Chunks[2][1] = ChunkY+1;

    Chunks[3][0] = ChunkX+1;
    Chunks[3][1] = ChunkY;

    Chunks[4][0] = ChunkX+1;
    Chunks[4][1] = ChunkY-1;

    Chunks[5][0] = ChunkX;
    Chunks[5][1] = ChunkY-1;

    Chunks[6][0] = ChunkX-1;
    Chunks[6][1] = ChunkY-1;

    Chunks[7][0] = ChunkX-1;
    Chunks[7][1] = ChunkY;

    Chunks[8][0] = ChunkX-1;
    Chunks[8][1] = ChunkY+1;

    ERL_NIF_TERM res = enif_make_list(env, 0);
    int ChunkResX, ChunkResY;
	for (int i = 0; i < 9; i++){
	    ChunkResX = Chunks[i][0];
	    ChunkResY = Chunks[i][1];
	    if(in_c_handler(ChunkX, ChunkY, MapR)){
	        res = enif_make_list_cell(env, enif_make_tuple2(env, enif_make_int(env, ChunkResX), enif_make_int(env, ChunkResY)), res);
	    }
	}

    return res;

}


static ERL_NIF_TERM move_body(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	double OldX, OldY, Xa, Ya, Xb, Yb, Gap, Speed;
	enif_get_double(env, argv[0], &OldX);
	enif_get_double(env, argv[1], &OldY);
	enif_get_double(env, argv[2], &Xa);
	enif_get_double(env, argv[3], &Ya);
	enif_get_double(env, argv[4], &Xb);
	enif_get_double(env, argv[5], &Yb);
	enif_get_double(env, argv[6], &Gap);
	enif_get_double(env, argv[7], &Speed);

	double Move, X, Y;
	if (Gap < Speed * 0.8) {
		Move = 1 - Gap / Speed;
		X = OldX + (Xa - OldX) * Move;
		Y = OldY + (Ya - OldY) * Move;
	}
	else{
		double Gap1 = Gap + (Gap / Speed * 10);
		double A = Speed / Gap1;
		Move = Speed / (Gap1 + Speed);
		double P1x = Xb + (Xa - Xb) * Move;
		double P1y = Yb + (Ya - Yb) * Move;
		X = P1x + (OldX - P1x) * A;
		Y = P1y + (OldY - P1y) * A;
	}
	return enif_make_tuple2(env, enif_make_double(env, X), enif_make_double(env, Y));
}


// 函数导出 ERL_NIF_DIRTY_JOB_CPU_BOUND
static ErlNifFunc nif_funcs[] =
{
	//一个函数定义hello，其参数个数为0，具体实现为c函数static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])。
	{"get_in_chunks", 4, get_in_chunks},
	{"get_xyz", 3, get_xyz},
	{"map_chunks", 2, map_chunks},
	{"view_chunks", 4, view_chunks},
	{"move_body", 8, move_body}

};

// ERL_NIF_INIT的第一个参数是该NIF的模块名，第二个参数是该模块包含的所有可供外部调用的函数定义数组
//   NIF的模块名必须与erlang module同名!!
ERL_NIF_INIT(map_math_nif, nif_funcs, NULL, NULL, NULL, NULL);

