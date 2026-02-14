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
#include "ores.testing/make_generation_context.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::testing {

using ores::utility::generation::generation_context;
using ores::utility::generation::generation_keys;

generation_context make_generation_context(database_helper& h) {
    return generation_context({
        {std::string(generation_keys::modified_by), h.db_user()},
        {std::string(generation_keys::tenant_id),
         h.tenant_id().to_string()}
    });
}

generation_context
make_generation_context(database_helper& h, std::uint64_t seed) {
    return generation_context(seed, {
        {std::string(generation_keys::modified_by), h.db_user()},
        {std::string(generation_keys::tenant_id),
         h.tenant_id().to_string()}
    });
}

generation_context make_generation_context(scoped_database_helper& h) {
    return generation_context({
        {std::string(generation_keys::modified_by), h.db_user()},
        {std::string(generation_keys::tenant_id),
         h.tenant_id().to_string()}
    });
}

generation_context
make_generation_context(scoped_database_helper& h, std::uint64_t seed) {
    return generation_context(seed, {
        {std::string(generation_keys::modified_by), h.db_user()},
        {std::string(generation_keys::tenant_id),
         h.tenant_id().to_string()}
    });
}

}
