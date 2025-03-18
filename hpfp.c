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
  
  if (exp == 0b11111){
    if (M == 0) return s ? INT_MIN : INT_MAX; // 양/음의 무한대 처리
    else return HPFP_NAN; // NaN 처리 
  }

  if (exp == 0b00000) return 0; // denormalized

  int E = exp - 15; // 지수 값
  int value = (M >> (10 - E)) + (1 << E);

  // 부호에 따라 적절한 값 반환 
  return s ? -value : value;
}

/** float -> hpfp로 변환 */
hpfp float_converter(float input) {
  // 특수값 처리 (normalized, denormalized, special values)
  if (input > HPFP_MAX) return HPFP_POS_INF;
  if (input < -HPFP_MAX) return HPFP_NEG_INF;
  if (input == 0) return HPFP_POS_ZERO;

  // signbit 결정
  unsigned short s = (input < 0) ? 1 : 0;
  if (s) input = -input; // 절대값 변환

  // 정수부/소수부 나누어 저장
  int inputInt = (int)input;
  float inputFrac = input - inputInt;

  // MSB 위치 탐색
  int count = 0;
  for (int i = 15; i >= 0; i--) {
      if ((inputInt >> i) & 1) break;
      count++;
  }
  if (count == 16) return HPFP_POS_ZERO;

  // Exponent
  unsigned short exp = (15 - count) + 15;

  // Mantissa
  unsigned short M = ((inputInt & ~(1 << (15 - count))) << (10 - (15 - count))) & 0x3FF;
  M |= ((int)(inputFrac * (1 << (10 - (15 - count)))) & 0x3FF);

  // 최종 16bit hpfp 반환
  return (s << 15) | (exp << 10) | M;
}

/** hpfp -> float로 변환 */
float hpfp_to_float_converter(hpfp input) {
  unsigned short s = (input >> 15) & 1;      // sign bit 추출
  unsigned short exp = (input >> 10) & 0x1F; // exponent bits 추출
  unsigned short M = input & 0x3FF;          // mantissa bits 추출

  if (exp == 0b11111) {
      if (M == 0) return s ? HPFP_NAN : HPFP_MAX; // 양/음의 무한대 처리
      else return HPFP_NAN; // NaN 처리
  }

  if (exp == 0 && M == 0) return 0.0f;

  int E = exp - 15; // 지수 값
  float value = (1.0f + (M / (float)(1 << 10))) * (1 << E);

  // 부호 적용 후 반환
  return s ? -value : value;
}

/** hpfp 덧셈 연산 */
hpfp addition_function(hpfp a, hpfp b) {
  // a, b 에 대해서 sign, exponent, fraction 추출
  unsigned short sA = (a >> 15) & 1;
  unsigned short expA = (a >> 10) & 0x1F;
  unsigned short fracA = a & 0x3FF;

  unsigned short sB = (b >> 15) & 1;
  unsigned short expB = (b >> 10) & 0x1F;
  unsigned short fracB = b & 0x3FF;

  // Special cases (NaN, Infinity)
  if ((expA == 0b11111 && fracA) || (expB == 0b11111 && fracB)) return HPFP_NAN;
  if (expA == 0b11111) return a;
  if (expB == 0b11111) return b;

  // Hidden bit 처리
  if (expA != 0) fracA |= (1 << 10); // 정규수인 경우 1 추가
  if (expB != 0) fracB |= (1 << 10); // 정규수인 경우 1 추가

  // 지수 정렬
  int shift = 0;
  unsigned short exp_out;
  if (expA > expB) {
      shift = expA - expB;
      exp_out = expA;
      fracB >>= shift; // 지수 차이만큼 fracB를 정렬
  } else if (expB > expA) {
      shift = expB - expA;
      exp_out = expB;
      fracA >>= shift; // 지수 차이만큼 fracA를 정렬
  } else {
      exp_out = expA;
  }

  // 부호에 따른 연산
  unsigned short s_out;
  unsigned int frac_out;
  if (sA == sB) { // 같은 부호이면 더하기
      frac_out = fracA + fracB;
      s_out = sA;
  } else { // 다른 부호이면 빼기
      if (fracA > fracB) {
          frac_out = fracA - fracB;
          s_out = sA;
      } else {
          frac_out = fracB - fracA;
          s_out = sB;
      }
  }

  // 오버플로우 처리 (자릿수 초과 시)
  if (frac_out & (1 << 11)) { 
      frac_out >>= 1;
      exp_out++;
  }

  // 정규화 과정
  while (frac_out && !(frac_out & (1 << 10))) {
      frac_out <<= 1;
      exp_out--;
  }

  // underflow 방지
  if (exp_out <= 0) {
      frac_out >>= (1 - exp_out);
      exp_out = 0;
  }

  // frac 정리
  frac_out &= 0x3FF;

  // 최종 hpfp 반환
  return (s_out << 15) | (exp_out << 10) | frac_out;
}

/** hpfp 곱셈 연산 */
hpfp multiply_function(hpfp a, hpfp b) {
  //필요한 변수 선언 
  //a, b 에 대해서 s, exp, frac 추출
  int sA = (a >> 15) & 1; // sign bit 추출
  int expA = (a >> 10) & 0x1F; //exponent bits 추출
  int fracA = a & 0x3FF; // significand bits 추출

  int sB = (b >> 15) & 1; // sign bit 추출
  int expB = (b >> 10) & 0x1F; //exponent bits 추출
  int fracB = b & 0x3FF; // significand bits 추출

  int s_out = 0;
  int exp_out = 0;
  int frac_out = 0;

  int output = 0;

  int i;

  //specialized
  if ((expA == 0b11111 && fracA != 0) || (expB == 0b11111 && fracB != 0)) {  
    return HPFP_NAN; // NaN 반환
  }

// a 또는 b가 무한대일 경우
if (expA == 0b11111 || expB == 0b11111) { 
    // 하나가 무한대이고, 다른 하나가 0이면 NaN 반환
    if ((expA == 0b11111 && expB == 0 && fracB == 0) || 
        (expB == 0b11111 && expA == 0 && fracA == 0)) {
        return HPFP_NAN; // NaN 반환
    }

    // 결과 부호 결정 후 Infinity 반환
    return (sA ^ sB) ? HPFP_NEG_INF : HPFP_POS_INF; // 양/음 무한대 반환
  }

  s_out = sA ^ sB;
  exp_out = expA + expB - 15;

  
  int frac_tempA = 0;
  int frac_tempB = 0;


  //leading 1 붙여서 frac 값 꺼내오기 
  frac_tempA = fracA + (1 << 10);
  frac_tempB = fracB + (1 << 10);

  //denormalized
  if (expA == 0b00000) frac_tempA = fracA;
  if (expB == 0b00000) frac_tempB = fracB;


  frac_out = frac_tempA * frac_tempB;

  if (frac_out & 0b1000000000000000000000) {
      exp_out++;
      frac_out >>= 1;
  }


  int round = 0;
  if ((frac_out & 0x03FF) > 0x0200) {
      round = 1;
  }

  output |= (s_out << 15) & 0x8000;
  output |= (exp_out << 10) & 0x7c00;
  output |= (((frac_out >> 10) + round) & 0x03FF);


  return output;

}

/** hpfp 비교 연산 */
char *comparison_function(hpfp a, hpfp b) {
  unsigned short sA = (a >> 15) & 1;
  unsigned short expA = (a >> 10) & 0x1F;
  unsigned short fracA = a & 0x3FF;
  
  unsigned short sB = (b >> 15) & 1;
  unsigned short expB = (b >> 10) & 0x1F;
  unsigned short fracB = b & 0x3FF;

  if ((expA == 0b11111 && fracA) || (expB == 0b11111 && fracB)) return "="; // NaN 처리
  if (a == b) return "=";
  
  if (sA != sB) return sA ? "<" : ">"; // 부호 비교
  
  if (expA != expB) return (expA > expB) ^ sA ? ">" : "<"; // 지수 비교
  
  return (fracA > fracB) ^ sA ? ">" : "<"; // 가수 비교
}

/** hpfp 값을 bits로 표시하기 */
char *hpfp_to_bits_converter(hpfp result) {
  char *bits = (char *)malloc(17);
  if (!bits) return NULL; // 메모리 할당 실패 시 NULL 반환

  for (int i = 0; i < 16; i++) {
      bits[i] = '0' + ((result >> (15 - i)) & 1);
  }
  bits[16] = '\0';
  
  return bits;
}

/** Example: hpfp(0100010011110011) -> float(4.95) -> float(59.4) -> hpfp(0101001101101100) */
char *hpfp_flipper(char *input) {
  hpfp input_H = 0, output_H = 0;
  float input_F = 0, output_F = 0;
  int tempInt = 0, intReversed = 0;
  int count = 0, max_count = 10;  // 소수점 이하 최대 10자리까지 허용
  float precision = 1.0;

  // hpfp 값으로 변환
  for (int i = 0; i < 16; i++) {
      input_H = (input_H << 1) | (input[i] - '0');
  }

  // hpfp -> float 변환
  input_F = hpfp_to_float_converter(input_H);

  // 소수점 이하 자릿수 찾기 (무한 루프 방지)
  float temp = input_F;
  while (count < max_count && (temp - (int)temp) > 0.00001) {
      temp *= 10;
      precision *= 10;
      count++;
  }

  // 정수 형태로 변환 후 뒤집기
  tempInt = (int)(input_F * precision);
  while (tempInt) {
      intReversed = intReversed * 10 + tempInt % 10;
      tempInt /= 10;
  }

  // 다시 원래 소수 형태로 변환
  output_F = (float)intReversed / precision;

  // float -> hpfp 변환 후 비트 문자열 반환
  output_H = float_converter(output_F);
  return hpfp_to_bits_converter(output_H);
}