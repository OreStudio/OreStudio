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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_KIND_REGISTRY_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_KIND_REGISTRY_HPP

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/vendor/quote_description.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief One kind's answer to "build this row's factory input", or why not.
 */
struct feed_build_outcome final {
    std::optional<feed::feed_build_input> input;
    /** The rejection message the on-demand start verb replies with verbatim. */
    std::string skip_reason;
};

/**
 * @brief One config row of one kind, in the shape every shared verb reads.
 */
struct feed_kind_candidate final {
    /** The row's own id (fx_spot_generation_config::id, ir_curve_generation_config::id, ...). */
    std::string feed_config_id;
    /** The market_data_generation_config row this config hangs under. */
    boost::uuids::uuid container_id;
    std::string source_name;
    /** Log and message label: the oresmd URI for FX, "ccy/index_family" for IR. */
    std::string display_name;
    bool enabled = false;
    bool auto_start = false;
    std::optional<boost::uuids::uuid> folder_id;
    /** The currency pair of an FX feed, base then quote. Other kinds leave it empty. */
    std::optional<std::pair<std::string, std::string>> fx_pair;
    /**
     * @brief The pillars of a curve feed, in the notation's own words.
     *
     * A feed that quotes one instrument leaves the list empty. A curve quotes one id for each of
     * its pillars, so the versions of the list are what an adapter is offered.
     */
    std::vector<ores::marketdata::vendor::ir_curve_pillar> pillars;

    /**
     * @brief Builds the kind's factory input, or says why it cannot.
     *
     * The adapter is the one the feed publishes through, and it carries the provider whose
     * notation writes the ids: that one value decides which vendor ids the feed quotes and
     * where its ticks travel.
     */
    std::function<feed_build_outcome(const feed::feed_adapter& adapter)> build_input;
};

struct feed_kind;

/** One config row tagged with the kind that produced it. */
struct feed_kind_row final {
    const feed_kind* kind = nullptr;
    feed_kind_candidate candidate;
};

/** One config row plus its container: what the start verbs read. */
struct feed_start_target final {
    feed_kind_row row;
    /** The provider the container stands for. */
    std::string container_provider_code;
    /** The container belongs to the system tenant: the installation's, not a tenant's. */
    bool container_is_system = false;
    bool container_found = false;
    bool container_enabled = false;

    /**
     * @brief The one startability gate: the row and its container are both
     * enabled.
     */
    [[nodiscard]] bool startable() const {
        return row.candidate.enabled && container_found && container_enabled;
    }
};

/** One start attempt's producer, or the reason it cannot be built. */
struct feed_start_attempt final {
    std::shared_ptr<ores::marketdata::domain::i_feed> feed;
    std::string failure;
};

/** The kind-neutral half of one simulate request; the parameters stay per-kind. */
struct feed_simulate_envelope final {
    int num_ticks = 0;
    int num_paths = 0;
    std::uint32_t seed = 0;
    std::function<std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>(
        std::uint32_t seed)>
        make_process;
};

struct feed_simulation_result final {
    bool success = false;
    std::string message;
    std::vector<std::vector<double>> paths;
};

/**
 * @brief The permission that starts and stops feeds, of every kind.
 *
 * Running a feed changes what the platform publishes, so it is not the read of the feed's
 * config. A caller needs this and the read of the configs the verb touches.
 */
inline constexpr std::string_view run_permission = "synthetic::feeds:run";

/**
 * @brief One registered asset class: its factory kind, its config permission,
 * its config rows, and its two simulate closures.
 *
 * A kind supplies only what is genuinely per-asset-class. Everything the
 * control plane does with a kind is registry code.
 */
struct feed_kind final {
    /** The factory kind string; must already be registered in factory(). */
    std::string kind;
    /** "synthetic::<kind>_generation_configs:read". */
    std::string config_permission;

    /**
     * This kind's config rows, optionally narrowed to one row id.
     *
     * @param feed_config_id empty for every row, else the row's own id.
     */
    std::function<std::vector<feed_kind_candidate>(const ores::database::context&,
                                                   const std::string& feed_config_id)>
        candidates;

    /** This kind's simulate subject. */
    std::string simulate_subject;
    /** Decodes this kind's own simulate request into the shared envelope. */
    std::function<std::optional<feed_simulate_envelope>(const ores::nats::message&)>
        decode_simulate;
    /** Replies with this kind's own typed simulate response. */
    std::function<void(
        ores::nats::service::client&, const ores::nats::message&, const feed_simulation_result&)>
        reply_simulate;
};

/**
 * @brief The per-kind feed registry: the one place the control plane asks
 * "what kinds are there, what may the caller read, and what does this kind's
 * config look like".
 *
 * Owns the two reads that are not per-kind but are needed by several verbs: the
 * market_data_generation_config container lookup (which carries enabled and
 * enabled flag) and the synthetic folder subtree walk. Because it owns them, no
 * handler includes an ores.synthetic.core repository header.
 */
class feed_kind_registry final {
public:
    /** Every folder id in the subtree rooted at the second argument. */
    using folder_subtree_fn = std::function<std::set<boost::uuids::uuid>(
        const ores::database::context&, const boost::uuids::uuid&)>;

    /** The market_data_generation_config row with this id, if visible. */
    using container_by_id_fn = std::function<std::optional<domain::market_data_generation_config>(
        const ores::database::context&, const boost::uuids::uuid&)>;

    /** Every market_data_generation_config row visible to the caller. */
    using containers_fn = std::function<std::vector<domain::market_data_generation_config>(
        const ores::database::context&)>;

    /**
     * @brief The adapter a feed of @p provider_code publishes through, read from the catalogue.
     *
     * Nothing when the catalogue serves no simulated source for the provider. The catalogue
     * belongs to the market data service, so the lookup reads it through that component's
     * client and not through this service's tables.
     */
    using adapter_lookup_fn =
        std::function<std::optional<feed::feed_adapter>(std::string_view provider_code)>;

    /**
     * @brief The three reads the registry does on every kind's behalf.
     *
     * None of them is per-kind: the container table is one table with one
     * provider_code column, and the folder hierarchy is one tree. Injecting
     * them is what keeps every handler free of an ores.synthetic.core
     * repository and what lets a stub-kind test run with no database.
     */
    struct deps final {
        const feed::feed_factory* factory = nullptr;
        folder_subtree_fn folder_subtree;
        container_by_id_fn container_by_id;
        containers_fn containers;
        /**
         * The adapter lookup. A registry without it builds every feed as a producer with no
         * adapter, which is what a test that names no catalogue wants; the service always
         * installs one, so a feed it starts publishes as an adapter.
         */
        adapter_lookup_fn adapter_of_provider;
    };

    /**
     * @brief The production registry: the default factory and the three repository reads.
     *
     * @param adapter_of_provider The catalogue lookup a feed start uses to find the adapter the
     * feed publishes through. Nothing builds every feed as a producer with no adapter, which is
     * what a test registry that names no catalogue wants.
     */
    explicit feed_kind_registry(adapter_lookup_fn adapter_of_provider = {});
    /** Test and composition seam. */
    explicit feed_kind_registry(deps d);

    /**
     * @brief Register one kind.
     *
     * @throws std::invalid_argument if the kind is empty, duplicates a
     * registered kind, lacks any of its functions, or names a factory kind
     * that factory() does not have.
     */
    void register_kind(feed_kind k);

    [[nodiscard]] const feed::feed_factory& factory() const {
        return *deps_.factory;
    }

    /** The one dispatch point for the config-plane's members. Null when unknown. */
    [[nodiscard]] const feed_kind* find(std::string_view kind) const;

    /** Every registered kind, sorted by kind string. */
    [[nodiscard]] std::vector<const feed_kind*> all() const;

    /** True only when the caller holds every registered kind's config permission. */
    [[nodiscard]] bool permits_all_configs(const ores::database::context& ctx) const;

    /** True when the caller may start and stop feeds. */
    [[nodiscard]] static bool permits_run(const ores::database::context& ctx);

    /**
     * @brief Whether the caller works in the system tenant.
     *
     * A system collection is the installation's: its feeds run once for every tenant, so
     * only the system tenant starts and stops them.
     */
    [[nodiscard]] static bool is_system_caller(const ores::database::context& ctx);

    /** The ids of the system collections the caller sees. */
    [[nodiscard]] std::set<boost::uuids::uuid>
    system_containers(const ores::database::context& ctx) const;

    /** Every config row of every kind. No container read. */
    [[nodiscard]] std::vector<feed_kind_row> rows(const ores::database::context& ctx) const;

    /** Every config row of every kind, each with its container resolved. */
    [[nodiscard]] std::vector<feed_start_target> targets(const ores::database::context& ctx) const;

    /** The one row the caller named, with its container resolved. */
    [[nodiscard]] std::optional<feed_start_target> resolve(const ores::database::context& ctx,
                                                           const std::string& feed_config_id) const;

    /** Every folder id in the subtree rooted at root_id, root_id included. */
    [[nodiscard]] std::set<boost::uuids::uuid>
    folder_subtree(const ores::database::context& ctx, const boost::uuids::uuid& root_id) const;

    /**
     * @brief Apply the startability gate, load the kind's build input and build
     * the producer.
     *
     * The outcome handling stays with the verb, because the on-demand verb
     * replies, the cascade counts and the boot walk logs.
     */
    [[nodiscard]] feed_start_attempt make_feed(const feed_start_target& target,
                                               const feed::feed_build_context& bctx) const;

    /**
     * @brief Reply "Unknown feed kind" on @p msg in the registry's own
     * registered response shape, and do nothing when no kind is registered.
     *
     * A kind the registry does not know has no typed response of its own, so
     * the verb cannot pick one; the registry can, because it holds the
     * registrations. Only the unknown-kind guard uses this.
     */
    void reply_unknown_kind(ores::nats::service::client& nats,
                            const ores::nats::message& msg,
                            const std::string& kind) const;


private:
    [[nodiscard]] std::optional<feed_start_target>
    make_target(const feed_kind& k,
                feed_kind_candidate c,
                const std::map<boost::uuids::uuid, domain::market_data_generation_config>*
                    containers_by_id) const;

    deps deps_;
    std::map<std::string, feed_kind> kinds_;
};

/**
 * @brief The process-wide registry: FX spot and IR curves, registered once.
 *
 * @param adapter_of_provider The catalogue lookup a feed start uses to find the adapter it
 * publishes through. The service passes its own; a caller that passes none builds every feed as
 * a producer with no adapter, which stores no tick.
 */
feed_kind_registry
make_default_feed_kind_registry(feed_kind_registry::adapter_lookup_fn adapter_of_provider = {});

/**
 * @brief The one simulate envelope: clamp, then step num_paths processes of
 * num_ticks steps each, path p seeded with seed + p.
 *
 * Kind-neutral and side-effect free apart from env.make_process, so it is
 * directly unit-testable with no NATS, no database and no registry.
 */
feed_simulation_result run_simulate_paths(const feed_simulate_envelope& env);

}

#endif
