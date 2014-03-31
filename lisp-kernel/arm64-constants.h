

#define uvector_ref 0x40
#define uvector_header 0x80
#define gvector_mask 0x20
#define cl_ivector_mask 0x01

#define define_uvector(name, val) \
  tag_##name = (uvector_ref|val), name##_header = (uvector_header|val)

#define define_ivector(name,val) define_uvector(name,(val<<1))
#define define_cl_ivector(name,val) define_uvector(name,((val<<1)|cl_ivector_mask))
#define define_gvector(name,val) define_uvector(name,(gvector_mask|val))

enum {
define_ivector(bignum,0),        
define_cl_ivector(s32_vector,0),
define_ivector(double_float,1),
define_cl_ivector(u32_vector,1),
define_ivector(complex_single_float,2),
define_cl_ivector(single_float_vector,2),
define_ivector(complex_double_float,3),	
define_cl_ivector(simple_string,3),
define_ivector(xcode_vector,4),
min_32_bit_ivector_header = bignum_header,
max_32_bit_ivector_header = xcode_vector_header,
define_ivector(macptr,5),        
define_cl_ivector(s64_vector,5),
define_ivector(dead_macptr,6),
define_cl_ivector(u64_vector,6),
define_cl_ivector(fixnum_vector,7),  
define_cl_ivector(double_float_vector,8),
define_cl_ivector(complex_single_float_vector,9),	
min_64_bit_ivector_header = macptr_header,
max_64_bit_ivector_header = complex_single_float_vector_header,
define_cl_ivector(s8_vector,10),
define_cl_ivector(u8_vector,11),                
min_8_bit_ivector_header = s8_vector_header,
max_8_bit_ivector_header = u8_vector_header,
define_cl_ivector(s16_vector,12),
define_cl_ivector(u16_vector,13),
min_16_bit_ivector_header = s16_vector_header,
max_16_bit_ivector_header = u16_vector_header,
define_cl_ivector(complex_double_float_vector,14),	
define_cl_ivector(bit_vector,15),
define_gvector(ratio,0),
define_gvector(complex,1),
define_gvector(function,2),
define_gvector(symbol,3),
define_gvector(catch_frame,4),
define_gvector(basic_stream,5),                                        
define_gvector(lock,6),
define_gvector(hash_vector,7),
define_gvector(pool,8),
define_gvector(weak,9),
define_gvector(package,10),
define_gvector(slot_vector,11),
define_gvector(instance,12),
define_gvector(struct,13),
define_gvector(istruct,14),
define_gvector(value_cell,15),
define_gvector(xfunction,16),
define_gvector(arrayH,29),        
define_gvector(vectorH,30),
define_gvector(simple_vector,31)

};
