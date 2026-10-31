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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_ADAPTER_TARGETS_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_ADAPTER_TARGETS_HPP

#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.synthetic.api/feeds/adapter_target.hpp"
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::service {

/**
 * @brief The target a generator publishes one quote through: the id in its provider's own
 * notation and the ingress subject the tick travels on.
 *
 * The simulator never spells an id itself, and neither does a feed a person starts: both take
 * the id from the notation of the provider the quote belongs to. The subject is the adapter
 * ingress, which is the family the market data ingest consumes, so a tick published this way is
 * mapped, stored and delivered like any other adapter's.
 *
 * Nothing when the provider has no notation written, or when its notation does not write
 * @p description.
 */
inline std::optional<feed::adapter_target>
adapter_target_of(std::string_view adapter_id,
                  std::string_view provider_code,
                  const ores::marketdata::vendor::quote_description& description,
                  std::shared_ptr<const feed::fault_state> faults = {}) {
    const auto* notation = ores::marketdata::vendor::notation_of(provider_code);
    if (notation == nullptr)
        return std::nullopt;
    const auto written = notation->write(description);
    if (!written)
        return std::nullopt;
    return feed::adapter_target{.id = *written,
                                .subject = ores::marketdata::domain::adapter_ingress_subject(
                                    adapter_id, written->external_id),
                                .faults = std::move(faults)};
}

}

#endif
