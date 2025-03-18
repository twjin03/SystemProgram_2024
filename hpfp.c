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

/** hpfp 덧셈 연산 */
hpfp addition_function(hpfp a, hpfp b){
  // 필요한 변수 선언
  // a, b 에 대해서 s, exp, frac 추출
  unsigned short sA = (a >> 15) & 1;      // sign bit 추출
  unsigned short expA = (a >> 10) & 0x1F; // exponent bits 추출
  unsigned short fracA = a & 0x3FF;       // significand bits 추출

  unsigned short sB = (b >> 15) & 1;      // sign bit 추출
  unsigned short expB = (b >> 10) & 0x1F; // exponent bits 추출
  unsigned short fracB = b & 0x3FF;       // significand bits 추출

  if ((expA == 0b11111 && fracA) || (expB == 0b11111 && fracB)) return HPFP_NAN; // NaN 처리
  if (expA == 0b11111) return a; // a가 무한대일 경우
  if (expB == 0b11111) return b; // b가 무한대일 경우  

  // 지수 값 
  int Ea = expA - 15;
  int Eb = expB - 15;

  // 정규화된 값이면 Mantissa에 leading 1 추가 
  fracA |= (expA ? 1 << 10 : 0);
  fracB |= (expB ? 1 << 10 : 0);

  // 지수 및 Mantissa 정렬 
  if (Ea > Eb) {
    fracB >>= (Ea - Eb);
    expB = expA;
  } else if (Ea < Eb) {
    fracA >>= (Eb - Ea);
    expA = expB;
  }

  // 부호에 따라 연산 수행 
  unsigned short s_out = sA; // 부호 일단 sA
  unsigned short frac_out = (sA == sB) ? (fracA + fracB) : (fracA > fracB ? fracA - fracB : fracB - fracA);
  // 부호가 같으면 덧셈, 부호가 다르면 절대값 큰 값에서 작은 값을 뺌 
  if (fracA < fracB) s_out = sB; // 큰 쪽의 부호를 유지 

  // normailzation
  while (frac_out && !(frac_out & (1 << 10))) {
    frac_out <<= 1;
    expA--;
  }
  frac_out &= 0x3FF; // Mantissa는 10비트만 사용하므로 상위 비트 삭제

  // 최종 hpfp값 반환
  return (s_out << 15) | ((expA + 15) << 10) | frac_out;

}

/** hpfp 곱셈 연산 */
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

  if ((expA == 0b11111 && fracA) || (expB == 0b11111 && fracB)) return HPFP_NAN; // NaN 처리
  if (((expA == 0b11111) && (expB == 0 && fracB == 0)) || ((expB == 0b11111) && (expA == 0 && fracA == 0))) return 0b0111110000000001; // 무한대 * 0 → NaN
  if (expA == 0b11111 || expB == 0b11111) return (sA ^ sB) ? HPFP_NEG_INF : HPFP_POS_INF; // 무한대 처리
  if ((expA == 0 && fracA == 0) || (expB == 0 && fracB == 0)) return HPFP_POS_ZERO; // 0 처리
  
  // 부호 및 지수 연산 
  unsigned short s_out = sA ^ sB;
  int exp_out = expA + expB - 15;

  // 정규화된 값이면 Mantissa에 leading 1 추가 
  fracA |= (expA ? 1 << 10 : 0);
  fracB |= (expB ? 1 << 10 : 0);

  // 가수 곱셈 
  unsigned int frac_out = (unsigned int)fracA * fracB;

  // normalization
  if (frac_out & (1 << 21)) {
    exp_out++;
    frac_out >>= 1;
  }
  
  frac_out &= 0x3FF; // Mantissa는 10비트만 사용하므로 상위 비트 삭제

  // 최종 hpfp값 반환
  return (s_out << 15) | ((expA + 15) << 10) | frac_out;
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