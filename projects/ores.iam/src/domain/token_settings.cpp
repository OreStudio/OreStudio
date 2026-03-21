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
#include "ores.iam/domain/token_settings.hpp"

namespace ores::iam::domain {

token_settings token_settings::load(
    variability::service::system_settings_service& svc) {

    token_settings result;
    result.access_lifetime_s =
        svc.get_int("iam.token.access_lifetime_seconds");
    result.party_selection_lifetime_s =
        svc.get_int("iam.token.party_selection_lifetime_seconds");
    result.max_session_s =
        svc.get_int("iam.token.max_session_seconds");
    result.refresh_threshold_pct =
        svc.get_int("iam.token.refresh_threshold_pct");
    return result;
}

}
