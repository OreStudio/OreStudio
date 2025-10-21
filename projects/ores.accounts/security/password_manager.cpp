#include "password_manager.hpp"
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/kdf.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include <vector>
#include <sstream>
#include <iomanip>
#include <cstring>

namespace ores {
namespace accounts {
namespace security {

// scrypt parameters - OWASP recommendations
constexpr uint64_t N = 1 << 14; // CPU/memory cost
constexpr uint32_t r = 8;       // Block size
constexpr uint32_t p = 1;       // Parallelization
constexpr size_t SALT_LEN = 16;
constexpr size_t HASH_LEN = 64;

// Helper to Base64 encode
std::string base64_encode(const std::vector<unsigned char>& data) {
    BIO *bio, *b64;
    BUF_MEM *bufferPtr;

    b64 = BIO_new(BIO_f_base64());
    bio = BIO_new(BIO_s_mem());
    bio = BIO_push(b64, bio);

    BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL);
    BIO_write(bio, data.data(), data.size());
    BIO_flush(bio);

    BIO_get_mem_ptr(bio, &bufferPtr);
    std::string encoded(bufferPtr->data, bufferPtr->length);
    BIO_free_all(bio);

    return encoded;
}

// Helper to Base64 decode
std::vector<unsigned char> base64_decode(const std::string& encoded) {
    BIO *bio, *b64;
    std::vector<unsigned char> decoded(encoded.length());
    int decoded_len = 0;

    b64 = BIO_new(BIO_f_base64());
    bio = BIO_new_mem_buf(encoded.c_str(), -1);
    bio = BIO_push(b64, bio);

    BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL);
    decoded_len = BIO_read(bio, decoded.data(), decoded.size());
    BIO_free_all(bio);

    decoded.resize(decoded_len);
    return decoded;
}


std::string PasswordManager::createPasswordHash(const std::string &password) {
    std::vector<unsigned char> salt(SALT_LEN);
    if (RAND_bytes(salt.data(), salt.size()) != 1) {
        // Handle error: random number generation failed
        return "";
    }

    std::vector<unsigned char> hash(HASH_LEN);
    if (EVP_PBE_scrypt(password.c_str(), password.length(), salt.data(), salt.size(), N, r, p, 0, hash.data(), hash.size()) != 1) {
        // Handle error: scrypt failed
        return "";
    }

    std::stringstream ss;
    ss << "$scrypt$ln=" << std::log2(N) << ",r=" << r << ",p=" << p << "$"
       << base64_encode(salt) << "$" << base64_encode(hash);

    return ss.str();
}

bool PasswordManager::verifyPasswordHash(const std::string &password, const std::string &hash_str) {
    std::stringstream ss(hash_str);
    std::string segment;
    std::vector<std::string> segments;

    while(std::getline(ss, segment, '$')) {
       segments.push_back(segment);
    }

    if (segments.size() != 5 || segments[1] != "scrypt") {
        return false; // Invalid format
    }

    // For simplicity, we assume parameters match the ones we use for hashing.
    // A more robust implementation would parse these from segments[2].

    auto salt = base64_decode(segments[3]);
    auto expected_hash = base64_decode(segments[4]);

    std::vector<unsigned char> actual_hash(HASH_LEN);
    if (EVP_PBE_scrypt(password.c_str(), password.length(), salt.data(), salt.size(), N, r, p, 0, actual_hash.data(), actual_hash.size()) != 1) {
        return false; // scrypt failed
    }

    return CRYPTO_memcmp(expected_hash.data(), actual_hash.data(), HASH_LEN) == 0;
}

} // namespace security
} // namespace accounts
} // namespace ores
