/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

// Prints a credential row the IAM tables accept, using the real hasher.
//
// The hasher is the only authority on the stored format, so seeding test
// credentials by reimplementing scrypt in another language would risk a subtly
// different hash. This links the actual library instead.
//
// Build and run:
//   scripts/seed-test-account.sh <username> <password> [party-id]

#include "ores.security/crypto/password_hasher.hpp"
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "usage: make-test-hash <username> <password>\n";
        return 2;
    }
    const std::string username = argv[1];
    const std::string password = argv[2];

    const auto hash = ores::security::crypto::password_hasher::hash(password);
    if (!ores::security::crypto::password_hasher::verify(password, hash)) {
        std::cerr << "self-check failed: hash does not verify\n";
        return 1;
    }

    std::cout << username << '\t' << hash << '\n';
    return 0;
}
