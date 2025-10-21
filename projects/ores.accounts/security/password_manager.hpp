#pragma once

#include <string>

namespace ores {
namespace accounts {
namespace security {

class PasswordManager {
public:
  static std::string createPasswordHash(const std::string &password);
  static bool verifyPasswordHash(const std::string &password,
                                 const std::string &hash);
};

} // namespace security
} // namespace accounts
} // namespace ores
