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
#ifndef ORES_MARKETDATA_CORE_VENDOR_PROVIDER_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_PROVIDER_NOTATION_HPP

#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/quote_description.hpp"
#include <expected>
#include <string>
#include <string_view>

namespace ores::marketdata::vendor {

/**
 * @brief Why a notation did not read or write an id.
 */
struct notation_error final {
    enum class kind {
        /// The id scheme is not the provider's.
        wrong_scheme,
        /// The field is not one the provider's notation knows for this quote.
        wrong_field,
        /// The id does not follow the provider's grammar.
        malformed,
        /// The id is valid for the provider but names something this description cannot hold.
        unsupported,
        /// The description is valid, but the provider's notation cannot write it so that it
        /// reads back as the same description.
        not_representable
    };

    kind reason = kind::malformed;
    std::string message;
};

/**
 * @brief How one provider writes the identity of a quote, and how to read it back.
 *
 * A notation turns the id scheme, the external id and the field of a vendor id into a
 * description of the quote, and back. It is the provider's own grammar and nothing else. It
 * knows nothing of the catalogue, of the mapping to an oresmd item, of a source, of NATS or of
 * how a tick comes to exist, so a live adapter can use it to read real ids and a simulator can
 * use it to write native ones.
 *
 * Two properties hold for every notation:
 * - read(write(d)) is d, for every description the notation writes;
 * - write(read(id)) is id, for every id the notation reads.
 *
 * A notation refuses what it could not read back. It never writes an id that would read as a
 * different description.
 */
class ORES_MARKETDATA_CORE_EXPORT provider_notation {
public:
    virtual ~provider_notation() = default;

    /// The provider code, such as bloomberg.
    [[nodiscard]] virtual std::string_view provider() const = 0;

    /// The id scheme of the provider's ids, such as bbg_ticker.
    [[nodiscard]] virtual std::string_view id_scheme() const = 0;

    /// The side that the provider's main price field carries.
    [[nodiscard]] virtual price_side primary_side() const = 0;

    /// What @p id describes, or why the notation does not read it.
    [[nodiscard]] virtual std::expected<quote_description, notation_error>
    read(const messaging::vendor_id& id) const = 0;

    /// The id that writes @p description, or why the notation does not write it.
    [[nodiscard]] virtual std::expected<messaging::vendor_id, notation_error>
    write(const quote_description& description) const = 0;
};

/**
 * @brief The notation of a provider, or nothing for a provider with none written.
 *
 * The notations are independent classes. This is the one place that lists them.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT const provider_notation*
notation_of(std::string_view provider);

}

#endif
