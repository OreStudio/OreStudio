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
#include "ores.marketdata.core/terms/provider_terms.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <utility>

namespace ores::marketdata::terms {

bool has_terms(const domain::provider& p) {
    return !p.terms_url.empty();
}

std::string terms_digest(const domain::provider& p) {
    return utility::crypto::sha256::hex_digest(p.terms_url + '\n' + p.terms_credit + '\n' +
                                               (p.allows_storage ? "store" : "no-store"));
}

bool terms_accepted(const domain::provider& p) {
    return !p.terms_accepted_digest.empty() && p.terms_accepted_digest == terms_digest(p);
}

bool may_fetch(const domain::provider& p) {
    return !has_terms(p) || (terms_accepted(p) && p.allows_storage);
}

domain::provider
accept_terms(domain::provider p, std::string who, std::chrono::system_clock::time_point when) {
    p.terms_accepted_digest = terms_digest(p);
    p.terms_accepted_by = std::move(who);
    p.terms_accepted_at = when;
    return p;
}

}
