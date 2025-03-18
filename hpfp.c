#include "hpfp.h"
#include <limits.h>
#include <stdlib.h>

#define HPFP_MAX 65535

/*
첫 줄에 denotes the number of inputs as int
following lines에는 int type 입력들이 포함됨

float type입력과 hpfp입력의 경우에도 같은 형식
*/

float my_pow(float base, int exponent)
{
  float result = 1.0;
  while (exponent > 0)
  {
    result *= base;
    exponent--;
  }
  return result;
}

float my_fmod(float x, float y)
{
  while (x >= y)
  {
    x -= y;
  }
  return x;
}

hpfp int_converter(int input)
{ /* convert int into hpfp */ // int를 hpfp로

  /*필요한 변수 선언*/
  hpfp Binary = 0;      // 인코딩된 결과를 binary로 표현하기 위한 변수
  unsigned short M = 0; // 유효숫자 값. 이 값은 x나 x/y 형태의 수 (x는 정수, y는 2의 제곱)
  unsigned short E = 0; // 지수의 정수값

  unsigned short s = 0;   // signbit
  unsigned short exp = 0; // exp

  // Bias는 exponant가 5bit인 경우 15임.

  int i; // 반복문 실행을 위한 변수

  /*범위 안의 수인지 확인*/
  /*case 확인하기 (normalized, demormalized, special values)*/
  if (input > HPFP_MAX || input < -HPFP_MAX)
  {
    /*special values인 경우(오버플로우)*/
    // 오류 처리 코드 작성
    if (input > HPFP_MAX)
    { // 양의 무한대를 나타내는 값 반환
      Binary = 0b0111110000000000;
    }
    else if (input < -HPFP_MAX)
    { // 음의 무한대를 나타내는 값 반환
      Binary = 0b1111110000000000;
    }
    else
    {
      // NaN을 나타내는 값 반환
      Binary = 0b0111110000000001;
    }
    return Binary;
  }

  if (input == 0)
  { // 0인 경우, denormalized
    /*For **int** 0, mark the result as **hpfp** +0.0*/

    Binary = 0b1000000000000000;
  }

  // 0이 아닌 denormalized인 경우는 int에서는 없음

  else if (input > 0)
  { // 양의 정수, normalized
    s = 0;
    // M값 찾기
    int count = 0;
    for (i = sizeof(unsigned short) * 8 - 1; i >= 0; i--)
    { // MSB부터 시작
      count++;
      if (((input >> i) & 1) == 1) // num의 i번째 비트를 추출하는 식{
        break;
    }
    // for문이 끝나면 count는 맨 처음 나타난 1의 위치를 담고 있음.
    for (i = 0; i <= sizeof(unsigned short) * 8 - 1 - count; i++)
    {
      // LSB부터 1바로 오른쪽까지 진행
      M |= (((input >> i) & 1) << i);
    }
    E = 16 - count;
    exp = E + 15;

    /*비트연산해서 적절하게 집어넣기*/
    M = M << (10 - (16 - count)); //*****확인 필요***

    Binary |= (s << 15);
    Binary |= (exp << 10);
    Binary |= M;
  }
  else if (input < 0)
  {        // 음의 정수, normalized
    s = 1; // 부호 비트를 1로 설정

    // 입력 값의 절댓값을 양수로 바꾸어 처리
    input = -input;

    int count = 0;
    for (int i = sizeof(unsigned short) * 8 - 1; i >= 0; i--)
    {
      count++;
      if (((input >> i) & 1) == 1)
        break;
    }
    for (int i = 0; i <= sizeof(unsigned short) * 8 - 1 - count; i++)
    {
      M |= (((input >> i) & 1) << i);
    }
    E = 16 - count;
    exp = E + 15;

    M = M << (10 - (16 - count));

    Binary |= (s << 15);
    Binary |= (exp << 10);
    Binary |= M;
  }

  else
  {
    /*정수가 아님. 오류처리 해야 함.*/
    Binary = 0;
  }
  return Binary;
}

int hpfp_to_int_converter(hpfp input)
{
  unsigned short s = (input >> 15) & 1;      // sign bit 추출
  unsigned short exp = (input >> 10) & 0x1F; // exponent bits 추출
  unsigned short M = input & 0x3FF;          // mantissa bits 추출

  int E = exp - 15;
  int value = 0;

  if (s == 0 && exp == 0b11111 && M == 0)
  { // 양의 무한대
    value = INT_MAX;
    return value;
  }

  if (s == 1 && exp == 0b11111 && M == 0)
  { // 음의 무한대
    value = INT_MIN;
    return value;
  }

  if (exp == 0b11111 && M != 0)
  { // NaN
    value = INT_MIN;
    return value;
  }
  if (exp == 0b00000)
  { // denormallized
    value = 0;
    return value;
  }

  if (exp != 0b00000 && exp != 0b11111)
  {
    if (s == 0)
    { // 양의 정수, normalized
      // value = (M >> (10 - E)) * (1 << E);
      value = (M >> (10 - E)) + (1 << E);
      return value;
    }
    else
    { //(s == 1), 음의 정수, normalized
      value = (-1) * (M >> (10 - E)) + (1 << E);
    }
  }

  return value;
}

hpfp float_converter(float input)
{ /* convert float into hpfp */ // float를 hpfp로
  // 정수부 소수부 나누어서 처리
  // 배열 통해 input 저장

  short int inputI_Binary[11] = {0}; // 정수부 저장
  short int inputF_Binary[5] = {0};  // 소수부 저장
  short int outputBinary[16] = {0};  // 최종 결과 hpfp

  int i = 0; // 반복문 실행을 위한 변수

  hpfp Binary = 0; // 인코딩된 결과를 binary로 표현하기 위한 변수

  /*이 밑 변수는 필요에 따라 수정할 예정*/
  // unsigned short M = 0; //유효숫자 값. 이 값은 x나 x/y 형태의 수 (x는 정수, y는 2의 제곱)
  unsigned short E = 0;   // 지수의 정수값
  unsigned short s = 0;   // signbit
  unsigned short exp = 0; // exp

  // Bias는 exponant가 5bit인 경우 15임.

  int inputInt = 0;
  float inputFrac = 0;

  /*범위 안의 수인지 확인*/
  /*case 확인하기 (normalized, demormalized, special values)*/
  if (input > HPFP_MAX || input < -HPFP_MAX)
  {
    /*special values인 경우(오버플로우)*/
    // 오류 처리 코드 작성
    if (input > HPFP_MAX)
    { // 양의 무한대를 나타내는 값 반환
      Binary = 0b0111110000000000;
    }
    else if (input < -HPFP_MAX)
    { // 음의 무한대를 나타내는 값 반환
      Binary = 0b1111110000000000;
    }
    else
    {
      // NaN을 나타내는 값 반환
      Binary = 0b0111110000000001;
    }
  }

  if (input == 0)
  { // 0인 경우, denormalized
    /*For **int** 0, mark the result as **hpfp** +0.0*/

    Binary = 0b1000000000000000;
    return Binary;
  }

  if (input < 0)
  {
    input = -input;
    s = 1; // 0으로 초기화되어있음
  }

  int cnt = -1;

  // 정수부분 구하기
  inputInt = (int)input;
  // 소수부분 구하기
  inputFrac = input - inputInt;

  // 정수부분
  for (i = 0; inputInt >= 1; i++)
  {
    inputI_Binary[i] = inputInt % 2;
    inputInt /= 2;
    cnt++;
    if (inputInt == 0)
      break;
  }

  // 소수부분
  float temp = inputFrac;
  for (i = 0; i < 5; i++)
  {
    temp *= 2;
    inputF_Binary[i] = (short int)temp;
    temp -= (short int)temp;
  }

  // E 구하기
  E = cnt;
  exp = E + 15; // E + Bias

  outputBinary[0] = s;

  // exp 대입
  for (i = 5; i >= 1; i--)
  {
    outputBinary[i] = exp % 2;
    exp /= 2;
  }

  // frac 대입
  for (i = 6; i < cnt + 6; i++)
  {
    outputBinary[i] = inputI_Binary[cnt - i + 5];
  }
  for (i = cnt + 6; i < cnt + 11; i++)
  {
    outputBinary[i] = inputF_Binary[i - cnt - 6];
  }

  int tmp;

  tmp = 1;
  for (i = 15; i >= 0; i--)
  {
    Binary += (outputBinary[i] * tmp);
    tmp *= 2;
  }
  tmp = 0;
  return Binary;
}

float hpfp_to_float_converter(hpfp input)
{
  unsigned short s = (input >> 15) & 1;      // sign bit 추출
  unsigned short exp = (input >> 10) & 0x1F; // exponent bits 추출
  unsigned short M = input & 0x3FF;          // mantissa bits 추출

  int E = exp - 15;
  float value = 0;
  float n = 1;

  if (s == 0 && exp == 0b11111 && M == 0)
  { // 양의 무한대
    value = INT_MAX;
    return value;
  }

  if (s == 1 && exp == 0b11111 && M == 0)
  { // 음의 무한대
    value = INT_MIN;
    return value;
  }

  if (exp == 0b11111 && M != 0)
  { // NaN
    value = INT_MIN;
    return value;
  }

  if (s == 0)
  {                                     // 양의 정수, normalized
    value = (M >> (10 - E)) + (1 << E); // 정수부분

    int k = 1;
    for (int i = 9 - E; i >= 0; i--)
    { // 소수부분
      value += (((M >> i) & 1) * (n / (1 << k)));
      k++;
    }
  }
  else if (s == 1)
  { // 음의 정수, normalized

    value = (M >> (10 - E)) + (1 << E); // 정수부분

    int k = 1;
    for (int i = 9 - E; i >= 0; i--)
    { // 소수부분
      value += (((M >> i) & 1) * (n / (1 << k)));
      k++;
    }
    value = -value; // 음수로
  }
  else if (exp == 0 && M == 0)
  {
    value = 0;
  }

  else
  {
    /*정수가 아님. 오류처리 해야 함.*/
    value = INT_MIN;
  }
  return value;
}

hpfp addition_function(hpfp a, hpfp b)
{
  // 필요한 변수 선언
  // a, b 에 대해서 s, exp, frac 추출
  unsigned short sA = (a >> 15) & 1;      // sign bit 추출
  unsigned short expA = (a >> 10) & 0x1F; // exponent bits 추출
  unsigned short fracA = a & 0x3FF;       // significand bits 추출

  unsigned short sB = (b >> 15) & 1;      // sign bit 추출
  unsigned short expB = (b >> 10) & 0x1F; // exponent bits 추출
  unsigned short fracB = b & 0x3FF;       // significand bits 추출

  unsigned short s_out = 0;
  unsigned short exp_out = 0;
  unsigned short frac_out = 0;

  unsigned short output = 0;

  int i;
  // specialized
  if ((expA == 0b11111 && fracA != 0) || (expB == 0b11111 && fracB != 0))
  {                            // a, b 둘 중 하나 NaN
    return 0b0111110000000001; // NaN
  }

  if ((expA == 0b11111 && fracA == 0) || (expB == 0b11111 && fracB == 0))
  { // a, b 둘 중 하나 이상 무한대
    if (expA == expB)
    { // 둘 다 무한대
      if (sA == sB)
      { // 부호 같음
        if (sA == 0)
          return 0b0111110000000000; // 둘 다 양의 무한대
        else
          return 0b1111110000000000; // 둘 다 음의 무한대
      }
      else
      {                            // sA != sB, 부호 다름
        return 0b0111110000000001; // NaN
      }
    }
    else
    { // 둘 중 하나만 무한대
      if (sA == 0 && expA == 0b11111 && fracA == 0)
      { // a가 양의 무한대
        if (expB == 0b11111 && fracB != 0)
        { // b가 NaN
          return 0b0111110000000001;
        }
        else
        { // b가 NaN이 아님
          return 0b0111110000000000;
        }
      }
      else if (sA == 1 && expA == 0b11111 && fracA == 0)
      { // a가 음의 무한대
        if (expB == 0b11111 && fracB != 0)
        { // b가 NaN
          return 0b0111110000000001;
        }
        else
        { // b가 NaN이 아님
          return 0b1111110000000000;
        }
      }
      else if (sB == 0 && expB == 0b11111 && fracB == 0)
      { // b가 양의 무한대
        if (expA == 0b11111 && fracA != 0)
        { // a가 NaN
          return 0b0111110000000001;
        }
        else
        { // a가 NaN이 아님
          return 0b0111110000000000;
        }
      }
      else
      { // b가 음의 무한대
        if (expA == 0b11111 && fracA != 0)
        { // a가 NaN
          return 0b0111110000000001;
        }
        else
        { // a가 NaN이 아님
          return 0b1111110000000000;
        }
      }
    }
  }

  // normalized
  // hpfp의 exp 값을 통해 E 값 추출
  int Ea = expA - 15;
  int Eb = expB - 15;

  int E_sub = 0;
  int fp_sub = 0; // a, b 두 값의 소수점 자리수 차

  unsigned short frac_tempA = 0;
  unsigned short frac_tempB = 0;

  // leading 1 붙여서 frac 값 꺼내오기
  frac_tempA = fracA + (1 << 10);
  frac_tempB = fracB + (1 << 10);

  // denormalized
  if (expA == 0b00000)
    frac_tempA = fracA;
  if (expB == 0b00000)
    frac_tempB = fracB;

  // E값 비교
  if (Ea == Eb)
  {                 // 두 값의 지수가 같음
    exp_out = expA; // 일단 초기값 저장
  }
  else
  { // 두 값의 지수가 다름
    if (Ea > Eb)
    {
      // E값의 차이를 계산
      E_sub = Ea - Eb;
      // E가 더 작은 변수의 E값 및 exp 값 수정
      Eb = Ea;
      expB = expA;
      exp_out = expA; // 일단 초기값 저장

      // 지수가 더 컸던 수는 그대로 두고
      // 지수가 더 작았던 수는 계산했던 E의 값의 차이만큼 shift right
      frac_tempB >>= E_sub;
    }
    else
    { // Ea < Eb
      // E값의 차이를 계산
      E_sub = Eb - Ea;
      // E가 더 작은 변수의 E값 및 exp 값 수정
      Ea = Eb;
      expA = expB;
      exp_out = expB; // 일단 초기값 저장

      // 지수가 더 컸던 수는 그대로 두고
      // 지수가 더 작았던 수는 계산했던 E의 값의 차이만큼 shift right
      frac_tempA >>= E_sub;
    }
  }
  // 부호에 따라 연산 수행
  unsigned short frac_add = 0;
  if (sA == sB)
  {
    if (sA == 0)
    { // 둘 다 양수
      s_out = 0;
      // 덧셈연산
      frac_add = frac_tempA + frac_tempB;
    }
    else
    { // 둘 다 음수
      s_out = 1;
      // 덧셈 연산
      frac_add = frac_tempA + frac_tempB;
    }
  }
  else
  { // sA != sB
    if (sA == 0)
    {
      if (fracA > fracB)
      { // 절대값 비교
        s_out = 0;
        // 뺄셈 연산
        frac_add = frac_tempA - frac_tempB;
      }
      else
      { // Ea < Eb
        s_out = 1;
        // 뺄셈 연산
        frac_add = frac_tempB - frac_tempA;
      }
    }
    else
    { // sB == 0
      if (fracA > fracB)
      { // 절대값 비교
        s_out = 1;
        // 뺄셈 연산
        frac_add = frac_tempA - frac_tempB;
      }
      else
      { // Ea < Eb
        s_out = 0;
        // 뺄셈 연산
        frac_add = frac_tempB - frac_tempA;
      }
    }
  }

  // 연산되어 나온 값을 가지고 normalize (frac_out 값 구하기)
  int count = 0;
  for (i = sizeof(unsigned short) * 8 - 1; i >= 0; i--)
  { // MSB부터 시작
    count++;
    if (((frac_add >> i) & 1) == 1) // num의 i번째 비트를 추출하는 식{
      break;
  }

  for (i = 0; i <= sizeof(unsigned short) * 8 - 1 - count; i++)
  {
    // LSB부터 1바로 오른쪽까지 진행
    frac_out |= (((frac_add >> i) & 1) << i);
  }

  int sft = 10 - (16 - count);
  if (sft >= 0)
  {
    frac_out = frac_out << (-6 + count);
  }
  else
    frac_out = frac_out >> (6 - count);

  fp_sub = 6 - count;
  exp_out = exp_out + fp_sub;

  output |= (s_out << 15);
  output |= (exp_out << 10);
  output |= frac_out;

  return output;
  // leading 1 붙여서 frac 값 꺼내오기
  // 지수가 더 컸던 수는 그대로 두고
  // 지수가 더 작았던 수는 계산했던 E의 값의 차이만큼 shift right
  // 덧셈연산
  // 연산되어 나온 값을 가지고 normalize (leading 1 고려해 output hpfp의 frac 계산)
  // sign bit 저장
  // output hpfp의 지수값 계산
  // frac 라운딩 거쳐서 최종 hpfp 값 반환
}

hpfp multiply_function(hpfp a, hpfp b)
{
  // 필요한 변수 선언
  // a, b 에 대해서 s, exp, frac 추출
  int sA = (a >> 15) & 1;      // sign bit 추출
  int expA = (a >> 10) & 0x1F; // exponent bits 추출
  int fracA = a & 0x3FF;       // significand bits 추출

  int sB = (b >> 15) & 1;      // sign bit 추출
  int expB = (b >> 10) & 0x1F; // exponent bits 추출
  int fracB = b & 0x3FF;       // significand bits 추출

  int s_out = 0;
  int exp_out = 0;
  int frac_out = 0;

  int output = 0;

  int i;

  // specialized
  if ((expA == 0b11111 && fracA != 0) || (expB == 0b11111 && fracB != 0))
  {                            // a, b 둘 중 하나 NaN
    return 0b0111110000000001; // NaN
  }

  if ((expA == 0b11111 && fracA == 0) || (expB == 0b11111 && fracB == 0))
  { // a, b 둘 중 하나 이상 무한대
    if (expA == expB)
    { // 둘 다 무한대
      if (sA == sB)
      {                            // 부호 같음
        return 0b0111110000000000; // 양의 무한대 반환
      }
      else
      {                            // sA != sB, 부호 다름
        return 0b1111110000000000; // 음의 무한대 반환
      }
    }
    else
    { // 둘 중 하나만 무한대
      if (sA == 0 && expA == 0b11111 && fracA == 0)
      { // a가 양의 무한대
        if (expB == 0b11111 && fracB != 0)
        {                            // b가 NaN
          return 0b0111110000000001; // NaN
        }
        else
        { // b가 NaN이 아님
          if (expB == 0 && fracB == 0)
          {                            // b가 0
            return 0b0111110000000001; // NaN
          }
          else
            return 0b0111110000000000; // 양의 무한대
        }
      }
      else if (sA == 1 && expA == 0b11111 && fracA == 0)
      { // a가 음의 무한대
        if (expB == 0b11111 && fracB != 0)
        {                            // b가 NaN
          return 0b0111110000000001; // NaN
        }
        else
        { // b가 NaN이 아님
          if (expB == 0 && fracB == 0)
          {                            // b가 0
            return 0b0111110000000001; // NaN
          }
          else
            return 0b1111110000000000; // 음의 무한대
        }
      }
      else if (sB == 0 && expB == 0b11111 && fracB == 0)
      { // b가 양의 무한대
        if (expA == 0b11111 && fracA != 0)
        { // a가 NaN
          return 0b0111110000000001;
        }
        else
        { // a가 NaN이 아님
          if (expA == 0 && fracA == 0)
          {                            // a가 0
            return 0b0111110000000001; // NaN
          }
          else
            return 0b0111110000000000; // 양의 무한대
        }
      }
      else
      { // b가 음의 무한대
        if (expA == 0b11111 && fracA != 0)
        { // a가 NaN
          return 0b0111110000000001;
        }
        else
        { // a가 NaN이 아님
          if (expA == 0 && fracA == 0)
          {                            // a가 0
            return 0b0111110000000001; // NaN
          }
          else
            return 0b1111110000000000; // 음의 무한대
        }
      }
    }
  }
  s_out = sA ^ sB;
  exp_out = expA + expB - 15;

  int frac_tempA = 0;
  int frac_tempB = 0;

  // leading 1 붙여서 frac 값 꺼내오기
  frac_tempA = fracA + (1 << 10);
  frac_tempB = fracB + (1 << 10);

  // denormalized
  if (expA == 0b00000)
    frac_tempA = fracA;
  if (expB == 0b00000)
    frac_tempB = fracB;

  frac_out = frac_tempA * frac_tempB;

  if (frac_out & 0b1000000000000000000000)
  {
    exp_out++;
    frac_out >>= 1;
  }

  int round = 0;
  if ((frac_out & 0x03FF) > 0x0200)
  {
    round = 1;
  }

  output |= (s_out << 15) & 0x8000;
  output |= (exp_out << 10) & 0x7c00;
  output |= (((frac_out >> 10) + round) & 0x03FF);

  return output;
}

char *comparison_function(hpfp a, hpfp b)
{
  // input a 배열에 저장
  // input b 배열에 저장

  short int hpfpA[16] = {0};
  short int hpfpB[16] = {0};

  int i;

  for (i = 15; i >= 0; i--)
  {
    hpfpA[15 - i] = (a >> i) & 1;
  }
  for (i = 15; i >= 0; i--)
  {
    hpfpB[15 - i] = (b >> i) & 1;
  }

  // input a, input b 가 normalized/denormalized/special 중 무엇인지 확인
  // a와 b의 exp 값 계산
  int tempA = 0;
  int tempB = 0;
  for (i = 1; i <= 5; i++)
  {
    tempA += hpfpA[i];
    tempB += hpfpB[i];
  }

  // input a와 input b 중에 NaN이 있는지 확인
  int nanA = 0;
  int nanB = 0;

  // a와 b의 frac 계산
  if (tempA == 5 || tempB == 5)
  {
    for (i = 6; i <= 15; i++)
    {
      nanA += hpfpA[i];
      nanB += hpfpB[i];
    }
    // a와 b 둘 중에 nan 값이 있을 경우
    if (nanA != 0 || nanB != 0)
    {
      return "=";
    }
  }

  /*== 반환하는 경우*/
  // 양의무한 == 양의 무한
  // 음의 무한 == 음의 무한
  // NaN == 아무거나

  /*> 반환하는 경우*/
  // 양의 무한  > 음의 무한
  // 양의 무한 > normal value

  /*< 반환하는 경우*/
  // 음의 무한 < normal value

  // a와 b 부호 비교
  if (hpfpA[0] > hpfpB[0])
  {
    return "<";
  }
  else if (hpfpA[0] < hpfpB[0])
  {
    return ">";
  }

  // 부호 같을 경우
  // a와 b 지수 비교
  else
  { // hpfpA[0] == hpfpB[0]
    for (i = 1; i <= 5; i++)
    {
      if (hpfpA[i] > hpfpB[i])
      {
        return ">";
      }
      else if (hpfpA[i] < hpfpB[i])
      {
        return "<";
      }
      else
        continue;
    }
  }

  // 지수 같을 경우
  // a와 b 가수 비교
  for (i = 6; i <= 15; i++)
  {
    if (hpfpA[i] > hpfpB[i])
    {
      return ">";
    }
    else if (hpfpA[i] < hpfpB[i])
    {
      return "<";
    }
    else
      continue;
  }
  // 가수까지 같으면 둘은 같은 숫자
  return "=";
}

char *hpfp_to_bits_converter(hpfp result)
{
  char *bits = (char *)malloc(16 + 1); // 비트 문자열 저장에 사용될 메모리 할당

  for (int i = 15; i >= 0; i--)
  {
    bits[15 - i] = ((result >> i) & 1) ? '1' : '0'; // 각 비트를 문자열에 저장
  }
  bits[16] = '\0'; // 문자열 끝 표시

  return bits;
}

char *hpfp_flipper(char *input)
{
  short int input_arr[16] = {0};
  hpfp input_H = 0;
  hpfp output_H = 0;
  float input_F = 0;
  float output_F = 0;

  float n = 1;
  int i;

  for (i = 0; i < 16; i++)
  {
    input_arr[i] = input[i] - '0'; // 문자를 정수로 변환하여 배열에 저장
  }

  // 배열을 hpfp에 저장
  int tmp;
  tmp = 1;
  for (i = 15; i >= 0; i--)
  {
    input_H += (input_arr[i] * tmp);
    tmp *= 2;
  }

  // float로 변환
  input_F = hpfp_to_float_converter(input_H);

  // 소수점 이하 자릿수 구하기
  float temp = input_F;
  int count = 0;

  temp = input_F * my_pow(10, count);
  temp = my_fmod(temp, 1.0);
  while (temp != 0)
  {
    count++;
    temp = input_F * my_pow(10, count);
    temp = my_fmod(temp, 1.0);
  }

  // count에 소수점 자릿수가 저장됨

  // 소수점 이하 자릿수를 이용해 실수를 정수 형태로 만들기
  float tempFloat = input_F;
  for (i = 0; i < count; i++)
  {
    tempFloat *= 10;
  }
  int tempInt = (int)tempFloat;

  // 정수상태에서 뒤집기
  int intReversed = 0; // filpped integer

  while (tempInt != 0)
  {
    intReversed *= 10;
    intReversed += tempInt % 10;
    tempInt /= 10;
  }

  // 구해놓은 소수점 이하 자릿수를 이용해 output 값 return
  output_F = (float)intReversed;

  for (i = 0; i < count; i++)
  {
    output_F /= 10;
  }

  // float를 hpfp로 변경
  output_H = float_converter(output_F);

  // hpfp를 char로 변경
  return hpfp_to_bits_converter(output_H);
}