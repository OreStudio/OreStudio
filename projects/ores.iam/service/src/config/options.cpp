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
#include "ores.iam.service/config/options.hpp"
#include <rfl.hpp>
#include <rfl/json.hpp>

namespace ores::iam::service::config {

/**
 * The signing key is not written out.
 *
 * This operator is what the startup "Configuration:" log line at debug level
 * uses, and the key signs every token this service issues: anyone who reads it
 * out of a log file can mint a session for any account. The rest of the
 * configuration stays, because a dump that omits the settings it is there to
 * record is not worth having.
 */
std::ostream& operator<<(std::ostream& s, const options& v) {
    auto redacted(v);
    redacted.jwt_private_key = "***";
    rfl::json::write(redacted, s);
    return s;
}

}
