#include "hpfp.h"
#include <limits.h>
#include <stdlib.h>

#define HPFP_MAX 65535

// special case, denormalized case 값 정의 
#define HPFP_POS_INF 0b0111110000000000
#define HPFP_NEG_INF 0b1111110000000000
#define HPFP_NAN     0b0111110000000001
#define HPFP_POS_ZERO 0b0000000000000000
#define HPFP_NEG_ZERO 0b1000000000000000

/** 제곱 계산 */
float my_pow(float base, int exponent){
  float result = 1.0;
  while (exponent > 0){
    result *= base;
    exponent--;
  }
  return result;
}

/** 모듈러 연산 */
float my_fmod(float x, float y){
  while (x >= y){
    x -= y;
  }
  return x;
}

/** int -> hpfp로 변환 */
hpfp int_converter(int input){ 
  // 특수값 처리 (normalized, demormalized, special values)
  if (input > HPFP_MAX) return HPFP_POS_INF;
  if (input < -HPFP_MAX) return HPFP_NEG_INF; 
  if (input == 0) return HPFP_POS_ZERO; // For **int** 0, mark the result as **hpfp** +0.0

  // signbit 결정
  unsigned short s = (input < 0) ? 1: 0; 
  if (s) input = -input; // 절대값 

  int count = 0; 
  for (int i = 15; i >= 0; i--){ // MSB부터 검사하여 처음 1이 등장하는 비트 위치 저장 
    if ((input >> i) & 1) break;
    count++;
  }

  // Exponent 설정 
  unsigned short exp = (15 - count) + 15; // 가장 먼저 등장한 1의 위치에 15(Bias)를 더함

  // 가장 먼저 등장한 1 이후의 비트들만 Mantissa로 저장 
  unsigned short M = (input & ((1 << (15 - count)) - 1)) << (10 - (15 - count));
  
  // 최종 16bit hpfp 반환 
  return (s << 15) | (exp << 10) | M;
}

/** hpfp -> int로 변환 */
int hpfp_to_int_converter(hpfp input){
  unsigned short s = (input >> 15) & 1;      // sign bit 추출
  unsigned short exp = (input >> 10) & 0x1F; // exponent bits 추출
  unsigned short M = input & 0x3FF;          // mantissa bits 추출
  
  if (exp == 0b1111){
    if (M == 0) return s ? INT_MIN : INT_MAX; // 양/음의 무한대 처리
    else return HPFP_NAN; // NaN 처리 
  }

  if (exp == 0b0000) return 0; // denormalized

  int E = exp - 15; // 지수 값
  int value = (M >> (10 - E)) + (1 << E);

  // 부호에 따라 적절한 값 반환 
  return s ? -value : value;
}

/** float -> hpfp로 변환 */
hpfp float_converter(float input){ 
  // 특수값 처리 (normalized, demormalized, special values)
  if (input > HPFP_MAX) return HPFP_POS_INF;
  if (input < -HPFP_MAX) return HPFP_NEG_INF; 
  if (input == 0) return HPFP_POS_ZERO; 

  // signbit 결정
  unsigned short s = (input < 0) ? 1: 0; 
  if (s) input = -input; // 절대값 

  // 정수부/소수부 나누어 저장 
  int inputInt = (int)input;
  float inputFrac = input - inputInt;

  int count = 0; 
  for (int i = 15; i >= 0; i--){ // MSB부터 검사하여 처음 1이 등장하는 비트 위치 저장 
    if ((inputInt >> i) & 1) break;
    count++;
  }

  // Exponent
  unsigned short exp = (15 - count) + 15; // 가장 먼저 등장한 1의 위치에 15(Bias)를 더함

  // Mantissa 
  unsigned short M = ((inputInt - (1 << count)) << (10 - count)) | (int)(inputFrac * (1 << (10 - count)));
  
  // 최종 16bit hpfp 반환 
  return (s << 15) | (exp << 10) | M;
}

/** hpfp -> float로 변환 */
float hpfp_to_float_converter(hpfp input){
  unsigned short s = (input >> 15) & 1;      // sign bit 추출
  unsigned short exp = (input >> 10) & 0x1F; // exponent bits 추출
  unsigned short M = input & 0x3FF;          // mantissa bits 추출

  if (exp == 0b1111){
    if (M == 0) return s ? INT_MIN : INT_MAX; // 양/음의 무한대 처리
    else return HPFP_NAN; // NaN 처리 
  }

  if (exp == 0 && M == 0) return 0.0f; 

  int E = exp - 15; // 지수 값
  float value = (1 << E) + (M / (float)(1 << 10));

  // 부호에 따라 적절한 값 반환 
  return s ? -value : value;
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