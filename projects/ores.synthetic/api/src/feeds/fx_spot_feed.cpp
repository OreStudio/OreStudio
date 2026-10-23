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
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <random>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.fx_spot_feed");
    return instance;
}

// Resolves the initial price from a real market_observation when cfg.price_source is "vintage",
// mirroring the deleted feed_controller::vintage_data_available() -- the config's own series
// (from its oresmd_uri), keyed on (source=vintage_source, its own datum URI, date=vintage_date).
//
// @throws vintage_data_missing_error if no matching observation is found.
double
resolve_vintage_initial_price(ores::nats::service::nats_client& auth_nats,
                              const ores::synthetic::domain::fx_spot_generation_config& cfg) {
    const auto missing_message = "No vintage data found for source=" + cfg.vintage_source +
                                 ", date=" + cfg.vintage_date + ", datum=" + cfg.oresmd_uri + ".";

    // The series and the row are found by the URIs the config's datum writes as: the
    // same codec the marketdata service's ingest loop applies to a tick's URI, so
    // the vintage read and the tick that follows it name one series.
    const auto datum = ores::marketdata::datum::oresmd_uri_codec::read(cfg.oresmd_uri);
    if (!datum)
        throw vintage_data_missing_error("oresmd URI '" + cfg.oresmd_uri + "': " + datum.error());
    const auto datum_uri = ores::marketdata::datum::oresmd_uri_codec::write(*datum).value();
    const auto series_uri =
        ores::marketdata::datum::oresmd_uri_codec::write(ores::marketdata::datum::series_of(*datum))
            .value();

    const auto found = find_vintage_observation(auth_nats,
                                                {},
                                                series_uri,
                                                datum_uri,
                                                cfg.vintage_source,
                                                cfg.vintage_date,
                                                missing_message,
                                                cfg.oresmd_uri,
                                                boost::uuids::to_string(cfg.party_id));
    if (!found)
        throw vintage_data_missing_error(found.error());
    return *found;
}

} // namespace

ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed>
make_fx_spot_feed(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& auth_nats,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg,
                  const std::vector<ores::synthetic::domain::gmm_component>& components,
                  ores::synthetic::domain::binding_mode binding_mode,
                  std::optional<adapter_target> adapter) {
    // The config states the pair twice, as two currencies and as a URI. They must agree.
    const auto expected =
        "oresmd://fx/" + cfg.base_currency_code +
        "?type=quote&instrument=fx_spot&quote=rate&ccy=" + cfg.quote_currency_code;
    if (cfg.oresmd_uri != expected)
        throw std::invalid_argument("make_fx_spot_feed: config '" + cfg.oresmd_uri +
                                    "' is not the quote of the pair " + cfg.base_currency_code +
                                    "/" + cfg.quote_currency_code + ": expected '" + expected +
                                    "'.");
    if (components.empty())
        throw std::invalid_argument("make_fx_spot_feed: config '" + cfg.oresmd_uri +
                                    "' has no GMM components.");

    std::vector<double> means, stdevs, weights;
    means.reserve(components.size());
    stdevs.reserve(components.size());
    weights.reserve(components.size());
    for (const auto& c : components) {
        means.push_back(c.mean);
        stdevs.push_back(c.stdev);
        weights.push_back(c.weight);
    }

    // "vintage" resolves the initial price from a real market_observation, overriding the stored
    // gmm_initial_price (0 for vintage configs, per the check constraint); "fixed" uses the
    // stored value as-is. See make_fx_spot_feed's doc comment for the vintage semantics.
    const double initial_price = cfg.price_source == "vintage" ?
                                     resolve_vintage_initial_price(auth_nats, cfg) :
                                     cfg.gmm_initial_price;

    // Persistent random_device so the OS entropy pool is not re-seeded between rapid
    // successive calls (which can produce equal values on some platforms when called on
    // separate temporaries).
    static std::random_device rd;
    const std::uint32_t seed = rd();
    BOOST_LOG_SEV(lg(), ores::logging::info)
        << "SYNTHETIC SEED: source='" << cfg.source_name << "' seed=" << seed;

    auto process =
        ores::analytics::quant::service::process_factory::make_process(cfg.process_type,
                                                                       std::move(means),
                                                                       std::move(stdevs),
                                                                       std::move(weights),
                                                                       initial_price,
                                                                       seed);

    return std::make_shared<fx_spot_feed>(nats,
                                          cfg.oresmd_uri,
                                          cfg.source_name,
                                          producer_subject(cfg.source_name, binding_mode),
                                          std::move(process),
                                          static_cast<double>(cfg.ticks_per_hour),
                                          std::move(adapter));
}

fx_spot_feed::fx_spot_feed(
    ores::nats::service::client& nats,
    std::string quote_uri,
    std::string source_name,
    std::string nats_subject,
    std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process,
    double ticks_per_hour,
    std::optional<adapter_target> adapter)
    : nats_(nats)
    , quote_uri_(std::move(quote_uri))
    , source_name_(std::move(source_name))
    , process_(std::move(process))
    , clock_(ticks_per_hour)
    , nats_subject_(std::move(nats_subject))
    , adapter_(std::move(adapter)) {

    if (!process_)
        throw std::invalid_argument("fx_spot_feed: process must not be null");
    if (ticks_per_hour <= 0.0)
        throw std::invalid_argument("fx_spot_feed: ticks_per_hour must be positive");

    namespace datum = ores::marketdata::datum;
    const auto read = datum::oresmd_uri_codec::read(quote_uri_);
    if (!read)
        throw std::invalid_argument("fx_spot_feed: '" + quote_uri_ +
                                    "' names no datum: " + read.error());
    if (read->type() != datum::instrument_type::fx_spot || read->is_series())
        throw std::invalid_argument("fx_spot_feed: '" + quote_uri_ + "' is not an fx_spot quote.");
    quote_uri_ = datum::oresmd_uri_codec::write(*read).value();
    series_uris_ = {datum::oresmd_uri_codec::write(datum::series_of(*read)).value()};
}

const std::string& fx_spot_feed::source_name() const {
    return source_name_;
}

const std::vector<std::string>& fx_spot_feed::series_uris() const {
    return series_uris_;
}

const std::string& fx_spot_feed::role() const {
    static const std::string empty;
    return empty;
}

void fx_spot_feed::start() {
    clock_.run(
        [this] {
            const auto value = std::format("{}", process_->next());
            const auto now = std::chrono::system_clock::now();
            const auto& codec = ores::nats::default_wire_codec();
            // A quote is perishable, so it goes out on core NATS without a wait for an
            // acknowledgement. A producer that waits limits its own rate to the round
            // trip of the broker.
            if (adapter_) {
                ores::marketdata::messaging::adapter_tick tick;
                tick.id_scheme = adapter_->id.id_scheme;
                tick.external_id = adapter_->id.external_id;
                tick.field = adapter_->id.field;
                tick.value = value;
                tick.vendor_time = now;
                tick.sequence = clock_.publish_count() + 1;
                nats_.publish(adapter_->subject, codec.encode(tick));
                return value;
            }
            ores::marketdata::messaging::market_tick tick;
            tick.oresmd_uri = quote_uri_;
            tick.value = value;
            tick.observation_time = now;
            tick.source = source_name_;
            nats_.publish(nats_subject_, codec.encode(tick));
            return value;
        },
        [this](std::uint64_t n, const std::string& value) {
            if (n == 1 || n % 100 == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "SYNTHETIC PUBLISH: subject='" << published_subject() << "' oresmd_uri='"
                    << quote_uri_ << "' count=" << n << " value=" << value;
            }
        },
        [this](const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error)
                << "SYNTHETIC PUBLISH FAILED: subject='" << published_subject() << "' oresmd_uri='"
                << quote_uri_ << "': " << ex.what();
        });
}

void fx_spot_feed::stop() {
    clock_.stop();
}

}
