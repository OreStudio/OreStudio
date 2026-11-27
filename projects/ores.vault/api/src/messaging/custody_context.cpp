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
 */
#include "ores.vault.api/messaging/custody_context.hpp"

namespace ores::vault::messaging {

std::string custody_context::write(const std::string& name, const std::string& owner) {
    return "vault.write|" + name + "|" + owner;
}

std::string
custody_context::delivery(const std::string& subject, const std::string& name, int key_version) {
    return subject + "|" + name + "|" + std::to_string(key_version);
}

std::string custody_context::challenge(const std::string& consumer,
                                       const std::string& challenge_id) {
    return "vault.challenge|" + consumer + "|" + challenge_id;
}

std::string custody_context::credential(const std::string& consumer,
                                        const std::string& challenge_id,
                                        const std::string& name) {
    return "vault.credential|" + consumer + "|" + challenge_id + "|" + name;
}

}
