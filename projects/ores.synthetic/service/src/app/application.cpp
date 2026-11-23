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
#include "ores.synthetic.service/app/application.hpp"
#include "../derived_fx_vol_adapter.hpp"
#include "../feed_kind_registry.hpp"
#include "../feed_node_journal.hpp"
#include "../fixing_inputs.hpp"
#include "../generator_fleet.hpp"
#include "../generator_runner.hpp"
#include "../registrar.hpp"
#include "../simulated_source_handler.hpp"
#include "../simulator_adapter.hpp"
#include "../simulator_offers.hpp"
#include "../source_control.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.synthetic.api/domain/step_subjects.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.core/repository/stopped_feed_node_repository.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.synthetic.service/messaging/event_registrar.hpp"
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <span>
#include <thread>
#include <utility>
#include <vector>

namespace ores::synthetic::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;
namespace feed = ores::synthetic::feed;

namespace {
constexpr std::string_view service_name = "ores.synthetic.service";
constexpr std::string_view service_version = ORES_VERSION;
} // namespace

// Named, and outside the anonymous namespace, because application::run() is a
// coroutine and its frame holds the fleet: a type with no linkage in the frame
// is what gcc refuses under -Wsubobject-linkage.
namespace detail {

// An adapter serves one source of the catalogue. Its id is the name of the source, and the provider
// of the source is the one whose notation its ticks use.
struct simulated_adapter final {
    std::string name;
    std::string provider_code;
    std::unique_ptr<generator_runner> runner;
    std::unique_ptr<simulator_adapter> adapter;
    /// An operator stopped it, so it is built but not running.
    bool stopped = false;
};

// The simulator serves each source of the catalogue whose kind is simulated, one adapter for each.
// The simulator is the implementation of that kind, so it is the one place that reads the kind.
// The catalogue belongs to marketdata, so the sources come through its client.
std::vector<simulated_adapter>
make_adapters(const std::vector<ores::marketdata::domain::feed>& sources,
              const ores::database::context& db_ctx,
              ores::nats::service::client& nats,
              generator_fleet& generators,
              const feed_kind_registry& registry,
              const ores::synthetic::feed::feed_build_context& build,
              const std::shared_ptr<ores::synthetic::feed::fault_state>& faults,
              const node_halted_fn& node_halted,
              const fixing_offer_inputs* fixings) {
    std::vector<simulated_adapter> out;
    for (const auto& source : sources) {
        if (source.session_code != "synthetic-local")
            continue;
        simulated_adapter one;
        one.name = source.name;
        one.provider_code = source.publisher_code;
        one.runner = std::make_unique<generator_runner>(
            [&registry,
             &build,
             ctx = db_ctx,
             faults,
             surfaces = &generators.surfaces(),
             name = source.name,
             provider = source.publisher_code,
             node_halted,
             fixings] {
                return system_generator_offers(
                    registry, ctx, build, name, provider, faults, surfaces, node_halted, fixings);
            },
            &generators.running_names());
        one.adapter = std::make_unique<simulator_adapter>(nats,
                                                          *one.runner,
                                                          source.name,
                                                          source.publisher_code,
                                                          simulator_adapter_timing{},
                                                          faults);
        out.push_back(std::move(one));
    }
    return out;
}

// The adapters of the simulator, started once marketdata answers. The services of an installation
// start in no fixed order, so the simulator asks again until the catalogue is readable. An
// operator stops and starts one through the source control; a source stopped before the service
// started is built but left stopped.
class adapter_fleet final : public ores::synthetic::service::source_control {
private:
    static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.service.app.adapter_fleet");
        return instance;
    }

public:
    adapter_fleet(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& svc_nats,
                  const ores::database::context& db_ctx,
                  generator_fleet& generators,
                  const feed_kind_registry& registry,
                  const ores::synthetic::feed::feed_build_context& build,
                  std::shared_ptr<ores::synthetic::feed::fault_state> faults,
                  std::shared_ptr<feed_node_journal> journal)
        : nats_(nats)
        , svc_nats_(svc_nats)
        , db_ctx_(db_ctx)
        , generators_(generators)
        , registry_(registry)
        , build_(build)
        , faults_(std::move(faults))
        , journal_(std::move(journal))
        , worker_([this](ores::platform::concurrency::stop_token stop) { run(stop); }) {}

    ~adapter_fleet() override {
        worker_.request_stop();
        if (worker_.joinable())
            worker_.join();
        for (auto& one : derived_)
            one->stop();
        for (auto& one : adapters_) {
            generators_.detach(*one.runner);
            one.adapter->stop();
            one.runner->stop_all();
        }
    }

    adapter_fleet(const adapter_fleet&) = delete;
    adapter_fleet& operator=(const adapter_fleet&) = delete;

    [[nodiscard]] std::vector<ores::synthetic::messaging::simulated_source>
    sources() const override {
        const std::lock_guard lock(mu_);
        std::vector<ores::synthetic::messaging::simulated_source> out;
        out.reserve(adapters_.size());
        for (const auto& one : adapters_) {
            const auto holds =
                one.runner->running_count_of(ores::synthetic::feed::fx_vol_surface_feed_kind);
            const auto effect = generators_.surfaces().effect_of_releasing(one.name);
            out.push_back({.source_name = one.name,
                           .provider_code = one.provider_code,
                           .stopped = one.stopped,
                           .served_feeds = static_cast<int>(one.runner->running_count() - holds),
                           .held_surfaces = static_cast<int>(effect.held),
                           .surfaces_only_held_here = static_cast<int>(effect.only_here)});
        }
        std::ranges::sort(out, {}, &ores::synthetic::messaging::simulated_source::source_name);
        return out;
    }

    bool stop(const std::string& source_name) override {
        const auto before = graph({});
        const auto done = stop_source(source_name);
        if (done)
            halt_what_the_stops_starve();
        journal_->say_changes(before, graph({}));
        return done;
    }

    bool start(const std::string& source_name) override {
        const auto before = graph({});
        const auto done = start_source(source_name);
        if (done)
            halt_what_the_stops_starve();
        journal_->say_changes(before, graph({}));
        return done;
    }

    bool stop_source(const std::string& source_name) {
        const std::lock_guard lock(mu_);
        auto* one = find(source_name);
        if (one == nullptr)
            return false;
        if (!one->stopped) {
            one->adapter->stop();
            one->runner->stop_all();
            one->stopped = true;
            ORES_LOG_SEV(lg(), info) << "Simulated source " << source_name << " stopped";
        }
        return true;
    }

    bool start_source(const std::string& source_name) {
        const std::lock_guard lock(mu_);
        auto* one = find(source_name);
        if (one == nullptr)
            return false;
        if (one->stopped) {
            one->adapter->start();
            one->stopped = false;
            ORES_LOG_SEV(lg(), info) << "Simulated source " << source_name << " started";
        }
        return true;
    }

    [[nodiscard]] std::optional<feed_node> node(const std::string& name) const override {
        const auto found = graph_node(name);
        if (!found)
            return std::nullopt;
        return feed_node{.name = found->name,
                         .kind = found->kind,
                         .provider_code = found->provider_code,
                         .stopped = node_stopped(found->name)};
    }

    bool stop_node(const std::string& name) override {
        const auto before = graph({});
        const auto done = set_node(name, true);
        journal_->say_changes(before, graph({}));
        return done;
    }

    bool start_node(const std::string& name) override {
        const auto before = graph({});
        const auto done = set_node(name, false);
        journal_->say_changes(before, graph({}));
        return done;
    }

    [[nodiscard]] feed_graph graph(const std::string& around_node) const override {
        auto whole = build_graph();
        std::set<std::string> stopped;
        {
            const std::lock_guard lock(nodes_mu_);
            stopped = stopped_nodes_;
        }
        {
            const std::lock_guard lock(mu_);
            for (const auto& one : adapters_)
                if (one.stopped)
                    stopped.insert(one.name);
        }
        derive_states(whole, stopped, journal_->diagnostics(), stop_reasons());
        return around_node.empty() ? whole : around(whole, around_node);
    }

private:
    // The graph of the system collections' feeds and the sources the service serves, without
    // states.
    [[nodiscard]] feed_graph build_graph() const {
        std::vector<graph_source> served;
        {
            const std::lock_guard lock(mu_);
            for (const auto& one : adapters_)
                served.push_back({.name = one.name, .provider_code = one.provider_code});
        }
        const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(db_ctx_);
        return build_feed_graph(registry_.targets(sys_ctx), served);
    }

    [[nodiscard]] std::optional<ores::synthetic::messaging::feed_graph_node>
    graph_node(const std::string& name) const {
        const auto whole = build_graph();
        const auto found = std::ranges::find(
            whole.nodes, name, &ores::synthetic::messaging::feed_graph_node::name);
        if (found == whole.nodes.end() || found->kind == node_kind::source)
            return std::nullopt;
        return *found;
    }

    bool set_node(const std::string& name, bool stopped) {
        const auto found = graph_node(name);
        if (!found)
            return false;
        {
            const std::lock_guard lock(nodes_mu_);
            if (stopped)
                stopped_nodes_.insert(name);
            else
                stopped_nodes_.erase(name);
        }
        halt_what_the_stops_starve();
        ORES_LOG_SEV(lg(), info) << "Feed node " << name << " (" << found->kind << ")"
                                 << (stopped ? " stopped" : " started");
        return true;
    }

    // The offers withhold every node the operators' stops leave not running, the stopped nodes
    // and every node they starve, so a surface whose curve stopped stops too rather than run on
    // the curve's last value. A diagnostic halts nothing: a surface withheld for failing would
    // never run again to clear it. Every running adapter then re-applies its demand, so the halts
    // land at once and lift at once.
    void halt_what_the_stops_starve() {
        auto whole = build_graph();
        std::set<std::string> stopped;
        {
            const std::lock_guard lock(nodes_mu_);
            stopped = stopped_nodes_;
        }
        {
            const std::lock_guard lock(mu_);
            for (const auto& one : adapters_)
                if (one.stopped)
                    stopped.insert(one.name);
        }
        derive_states(whole, stopped);
        std::set<std::string> halted;
        for (const auto& n : whole.nodes)
            if (n.state != node_state::running)
                halted.insert(n.name);
        {
            const std::lock_guard lock(nodes_mu_);
            halted_ = std::move(halted);
        }
        const std::lock_guard lock(mu_);
        for (auto& one : adapters_)
            if (!one.stopped)
                one.runner->reapply();
    }

    // Stopped is what an operator stopped; halted is that and every node it starves, which the
    // offers withhold.
    [[nodiscard]] bool node_stopped(const std::string& name) const {
        const std::lock_guard lock(nodes_mu_);
        return stopped_nodes_.contains(name);
    }

    [[nodiscard]] bool node_halted(const std::string& name) const {
        const std::lock_guard lock(nodes_mu_);
        return halted_.contains(name);
    }

    simulated_adapter* find(const std::string& source_name) {
        const auto found = std::ranges::find(adapters_, source_name, &simulated_adapter::name);
        return found == adapters_.end() ? nullptr : &*found;
    }

    // Who stopped each stopped node and why, as its stored row says.
    std::map<std::string, std::string> stop_reasons() const {
        std::map<std::string, std::string> out;
        const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(db_ctx_);
        for (const auto& row :
             ores::synthetic::repository::stopped_feed_node_repository().read_latest(sys_ctx))
            out.emplace(row.node_name,
                        "Stopped by " + row.modified_by + " (" + row.change_reason_code + ")" +
                            (row.change_commentary.empty() ? std::string(".") :
                                                             ": " + row.change_commentary));
        return out;
    }

    // The nodes an operator stopped, which stay stopped across a restart: the sources, or the
    // others.
    std::set<std::string> stopped_rows(bool sources) const {
        std::set<std::string> out;
        const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(db_ctx_);
        for (const auto& row :
             ores::synthetic::repository::stopped_feed_node_repository().read_latest(sys_ctx))
            if ((row.kind == node_kind::source) == sources)
                out.insert(row.node_name);
        return out;
    }

    // Builds and starts every adapter before any is kept, so a failed start leaves none running.
    // A source an operator stopped is built and kept, but not started.
    void start_all(const std::vector<ores::marketdata::domain::feed>& sources) {
        // The simulated fixing publisher reads each pair's cut and the days the market holds.
        fixing_inputs_ = read_fixing_offer_inputs(db_ctx_, svc_nats_);
        {
            const std::lock_guard lock(nodes_mu_);
            stopped_nodes_ = stopped_rows(false);
        }
        // Seeded before the adapters are built, so no halted node is offered even once; the
        // sources' own stops join once the adapters are in.
        halt_what_the_stops_starve();
        auto built = make_adapters(
            sources,
            db_ctx_,
            nats_,
            generators_,
            registry_,
            build_,
            faults_,
            [this](const std::string& name) { return node_halted(name); },
            &fixing_inputs_);
        const auto stopped = stopped_rows(true);
        std::size_t attempted = 0;
        try {
            for (auto& one : built) {
                ++attempted;
                if (stopped.contains(one.name)) {
                    one.stopped = true;
                    ORES_LOG_SEV(lg(), info) << "Simulated source " << one.name
                                             << " stays stopped, as an operator left it";
                    continue;
                }
                one.adapter->start();
            }
        } catch (...) {
            // The adapter that threw is stopped too, because it may be half started.
            for (std::size_t i = 0; i < attempted; ++i) {
                try {
                    built[i].adapter->stop();
                    built[i].runner->stop_all();
                } catch (const std::exception& e) {
                    ORES_LOG_SEV(lg(), warn) << "An adapter did not stop cleanly: " << e.what();
                }
            }
            throw;
        }
        {
            const std::lock_guard lock(mu_);
            adapters_ = std::move(built);
        }
        for (auto& one : adapters_)
            generators_.attach(*one.runner);
        // The stops an operator left read back from the table halt what they starve.
        halt_what_the_stops_starve();
        // A derived source serves what the store already holds, so it runs beside the
        // simulator on the same catalogue: one for each feed of its session.
        for (const auto& source : sources) {
            if (source.session_code != "derived-local")
                continue;
            auto one = std::make_unique<ores::synthetic::service::derived_fx_vol_adapter>(
                nats_, svc_nats_, db_ctx_, source.name, source.publisher_code, source.id);
            one->start();
            derived_.push_back(std::move(one));
        }
        if (adapters_.empty())
            ORES_LOG_SEV(lg(), warn)
                << "The catalogue holds no simulated source, so the simulator serves no adapter.";
        else
            ORES_LOG_SEV(lg(), info)
                << adapters_.size() << " simulated source(s) served as adapters";
    }

    void run(ores::platform::concurrency::stop_token stop) {
        using namespace std::chrono_literals;
        auto wait = 2s;
        while (!stop.stop_requested()) {
            std::string failure;
            try {
                ores::marketdata::client::market_data_client client(svc_nats_);
                const auto sources = client.list_feeds();
                if (sources) {
                    start_all(*sources);
                    return;
                }
                failure = sources.error();
                if (failure.find("denied") != std::string::npos)
                    failure += " (an environment populated before the grant of "
                               "market_data_sources:read needs the IAM roles populated again)";
            } catch (const std::exception& e) {
                failure = e.what();
            }
            ORES_LOG_SEV(lg(), warn) << "The simulator serves no adapter yet: " << failure
                                     << ". Trying again in " << wait.count() << " s.";
            ores::platform::concurrency::wait_for(
                stop, std::chrono::duration_cast<std::chrono::milliseconds>(wait));
            wait = std::min(wait * 2, std::chrono::seconds(30));
        }
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& svc_nats_;
    ores::database::context db_ctx_;
    generator_fleet& generators_;
    std::vector<std::unique_ptr<ores::synthetic::service::derived_fx_vol_adapter>> derived_;
    const feed_kind_registry& registry_;
    const ores::synthetic::feed::feed_build_context& build_;
    std::shared_ptr<ores::synthetic::feed::fault_state> faults_;
    std::shared_ptr<feed_node_journal> journal_;
    /// What the simulated fixing publisher's offers read: each pair's cut and the held days.
    fixing_offer_inputs fixing_inputs_;
    // Guards the adapters against an operator's stop or start while the worker builds them.
    mutable std::mutex mu_;
    std::vector<simulated_adapter> adapters_;
    mutable std::mutex nodes_mu_;
    std::set<std::string> stopped_nodes_;
    std::set<std::string> halted_;
    ores::platform::concurrency::jthread worker_;
};

/// Stops the scheduler when the run leaves, however it leaves.
struct stop_scheduler_on_exit final {
    ores::analytics::quant::service::tick_scheduler& scheduler;
    ~stop_scheduler_on_exit() {
        scheduler.stop();
    }
};
} // namespace detail

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_min = static_cast<std::size_t>(db_opts.pool_min),
                                       .pool_max = static_cast<std::size_t>(db_opts.pool_max),
                                       .pool_idle_seconds =
                                           static_cast<std::size_t>(db_opts.pool_idle),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    ORES_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.synthetic.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // Authenticated client for service-to-service calls. The synthetic service
    // talks to the marketdata service (series + observations), all of which
    // require authentication and the marketdata permissions granted to the
    // SyntheticService IAM role. The token provider authenticates with the
    // service's own database account credentials, and is kept beside the client
    // because a provisioning step enters a tenant with it.
    auto service_token = ores::iam::client::make_service_token_provider(
        nats, cfg.database.user, cfg.database.password);
    ores::nats::service::nats_client svc_nats(nats, service_token);

    auto db_ctx = make_context(cfg.database);

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    auto generated_event_subs =
        messaging::event_registrar::register_event_mappings(event_source, event_bus, nats);
    event_source.start();
    ORES_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    try {
        auto admin = nats.make_admin();
        // A tick is perishable and goes out on core NATS. The two streams that
        // used to store every tick for seven days had no consumer, and each
        // publish waited for their acknowledgement. A server that still holds them
        // from an earlier run loses them here. The delete is idempotent.
        admin.delete_stream(nats.make_stream_name("synthetic_ticks"));
        admin.delete_stream(nats.make_stream_name("synthetic_sandbox_ticks"));
        // synthetic_steps is not ensured here. The service's own registrar
        // declares it, from the same table it subscribes, so the stream's
        // coverage and the consumer's filters cannot disagree.
        ORES_LOG_SEV(lg(), info) << "JetStream streams ready: the tick streams are retired";
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "Failed to ensure JetStream stream: " << e.what();
        throw;
    }

    // The source a feed publishes as, read from the catalogue through its own client: a start
    // the demand set does not already serve still needs an adapter, or the ingest drops every
    // tick it makes. A provider the catalogue serves no simulated source for has none, and a
    // feed under its collection is refused rather than started silently.
    std::function<std::optional<feed::feed_adapter>(std::string_view)> adapter_of_provider =
        [&svc_nats](std::string_view provider_code) -> std::optional<feed::feed_adapter> {
        try {
            ores::marketdata::client::market_data_client client(svc_nats);
            const auto sources = client.list_feeds();
            if (!sources)
                return std::nullopt;
            for (const auto& source : *sources)
                if (source.publisher_code == provider_code &&
                    source.session_code == "synthetic-local" && source.is_default)
                    return feed::feed_adapter{.id = source.name,
                                              .provider_code = std::string(provider_code)};
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), warn) << "The adapter of provider '" << provider_code
                                     << "' could not be read: " << e.what();
        }
        return std::nullopt;
    };

    // The one per-kind registry: the config-plane dispatch table every
    // control-plane verb reads.
    const auto registry = make_default_feed_kind_registry(adapter_of_provider);

    // One loop for the whole run. It owns the market, the regime chain and the
    // heat, and decides when every feed ticks, so a busy market lifts every feed
    // at once and one seed reproduces the run.
    // The heat adds half a rate per event and fades at twenty a second, so its
    // loop gain is a fortieth. Zero jump would leave the heat inert, so the
    // default is explicit here.
    ores::analytics::quant::service::steady_run_clock run_clock;
    const ores::analytics::quant::domain::market_config market{.wake_rate = 1.0 / 600.0,
                                                               .calm_rate = 1.0 / 300.0,
                                                               .active_multiplier = 2.0,
                                                               .jump = 0.5,
                                                               .decay = 20.0,
                                                               .speed = cfg.market_speed};
    ORES_LOG_SEV(lg(), info) << "SYNTHETIC RUN SEED: " << cfg.run_seed
                             << ", MARKET SPEED: " << cfg.market_speed;
    // The run's one market: its loop decides when every feed ticks, its noise is the
    // correlation between them, and its values are what they publish for each other. The
    // aggregate owns all three, so the driver hook it installs points at what it outlives.
    // The board holds a curve for each currency, a spot for each pair and the rate of each
    // index a curve fixes, so it is sized past the default.
    ores::analytics::quant::service::run_market run_market_of_the_run(
        run_clock, market, cfg.run_seed, 512);
    auto& scheduler = run_market_of_the_run.loop();
    scheduler.set_hooks([](std::uint64_t, const std::string&) {},
                        [](const std::exception& ex) {
                            ORES_LOG_SEV(lg(), error) << "SYNTHETIC TICK FAILED: " << ex.what();
                        });
    /*
     * The market is built here, before the loop starts, for two reasons. A block's loadings
     * are written in one pass while the market is made, so it cannot gain one later; and the
     * loop reads its driver source without a lock, so the source must be in place before the
     * loop runs.
     *
     * A surface hangs off the spot feed its config names: the block's anchor is that feed's
     * series, so the surface's forward variances, skew and curvature load on the very series
     * the spot publishes, the same feed whose level the surface reads. A surface that names no
     * spot feed has no anchor, and it is left out with a warning rather than started to move on
     * its own.
     */
    {
        const auto rows = registry.rows(db_ctx);
        std::map<std::string, std::uint64_t> spot_series;
        for (const auto& row : rows)
            if (row.kind->kind == feed::fx_spot_feed_kind)
                spot_series[row.candidate.source_name] =
                    ores::analytics::quant::math::series_key(row.candidate.source_name);

        for (const auto& row : rows) {
            if (!row.candidate.block)
                continue;
            const auto anchor = spot_series.find(row.candidate.inputs.spot);
            if (anchor == spot_series.end()) {
                ORES_LOG_SEV(lg(), warn)
                    << "Synthetic surface '" << row.candidate.source_name
                    << "' names no spot feed by its URI and provider, so it is not in the run's "
                    << "market";
                continue;
            }
            try {
                run_market_of_the_run.add_block(anchor->second,
                                                row.candidate.source_name,
                                                row.candidate.block->segments,
                                                row.candidate.block->spot_variance,
                                                row.candidate.block->spot_skew,
                                                row.candidate.block->neighbour);
            } catch (const std::exception& e) {
                ORES_LOG_SEV(lg(), warn) << "Synthetic surface '" << row.candidate.source_name
                                         << "' has no block: " << e.what();
            }
        }
        // A second is the finest gap this run can produce until the arrival laws are read
        // here. Every configured rate is far slower, so no tick is interpolated.
        run_market_of_the_run.build(3600.0, 0.0, 1.0);
        ORES_LOG_SEV(lg(), info) << "SYNTHETIC MARKET: "
                                 << run_market_of_the_run.noise().factors.size()
                                 << " factor(s) over "
                                 << run_market_of_the_run.noise().series_id.size() << " series";
    }

    ores::platform::concurrency::jthread scheduler_thread([&scheduler] { scheduler.run(); });

    // The scheduler runs until something stops it, so this guard stops it on
    // every exit path, including a throw. Declared after the thread, it is
    // destroyed first, which is what makes the thread joinable when its own
    // destructor runs: a startup failure reports itself instead of hanging
    // until systemd kills the process.
    detail::stop_scheduler_on_exit stop_on_exit{scheduler};

    // The simulator is an adapter of the market data service for each simulated source. It
    // serves the system collections of the provider of the source, and it starts a generator for
    // each id that the demand names.
    // Each feed node's messages go to telemetry, tagged with its name, so its page reads its
    // log by its name.
    const auto journal = std::make_shared<ores::synthetic::service::feed_node_journal>(
        [&nats](const std::string& node, std::string_view level, const std::string& message) {
            ores::telemetry::messaging::publish_log_entries_request req;
            req.source_name = "ores.synthetic.service";
            req.tag = node;
            req.entries.push_back(
                {.level = std::string(level),
                 .timestamp_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                                     std::chrono::system_clock::now().time_since_epoch())
                                     .count(),
                 .component = "ores.synthetic.feed_graph",
                 .message = message});
            nats.publish(ores::telemetry::messaging::publish_log_entries_request::nats_subject,
                         ores::nats::default_wire_codec().encode(req));
        });
    const ores::synthetic::feed::feed_build_context adapter_build{.nats = nats,
                                                                  .auth_nats = svc_nats,
                                                                  .market = run_market_of_the_run,
                                                                  .diagnostics = journal};
    const auto faults = std::make_shared<ores::synthetic::feed::fault_state>();
    // Every runner of the service claims its feeds here, so one set answers what runs now and
    // one stop reaches a feed whichever run holds it. It outlives the fleet that owns the runners.
    generator_fleet generators;
    detail::adapter_fleet fleet(
        nats, svc_nats, db_ctx, generators, registry, adapter_build, faults, journal);

    // Bound before the await, so no lambda of the call lives in this frame; see run().
    auto service_run = ores::service::service::run(
        io_ctx,
        nats,
        db_ctx,
        "ores.synthetic.service",
        [&generators, faults, &svc_nats, service_token, &registry, &run_market_of_the_run, &fleet](
            auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs =
                ores::synthetic::service::registrar::register_handlers(n,
                                                                       svc_nats,
                                                                       service_token,
                                                                       generators,
                                                                       faults,
                                                                       c,
                                                                       v,
                                                                       registry,
                                                                       run_market_of_the_run);
            subs.insert(subs.end(),
                        std::make_move_iterator(market_subs.begin()),
                        std::make_move_iterator(market_subs.end()));
            // The simulated sources are the fleet's, which lives here rather than in the
            // registrar, so their verbs are subscribed here.
            using ores::synthetic::service::simulated_source_handler;
            namespace msgs = ores::synthetic::messaging;
            constexpr auto queue = "ores.synthetic.service";
            subs.push_back(
                n.queue_subscribe(std::string(msgs::list_simulated_sources_request::nats_subject),
                                  queue,
                                  [&n, c, v, &fleet](ores::nats::message msg) {
                                      simulated_source_handler(n, c, v, fleet).list(std::move(msg));
                                  }));
            subs.push_back(n.queue_subscribe(
                std::string(msgs::get_feed_graph_request::nats_subject),
                queue,
                [&n, c, v, &fleet](ores::nats::message msg) {
                    simulated_source_handler(n, c, v, fleet).graph(std::move(msg));
                }));
            subs.push_back(
                n.queue_subscribe(std::string(msgs::stop_feed_node_request::nats_subject),
                                  queue,
                                  [&n, c, v, &fleet](ores::nats::message msg) {
                                      simulated_source_handler(n, c, v, fleet).stop(std::move(msg));
                                  }));
            subs.push_back(n.queue_subscribe(
                std::string(msgs::start_feed_node_request::nats_subject),
                queue,
                [&n, c, v, &fleet](ores::nats::message msg) {
                    simulated_source_handler(n, c, v, fleet).start(std::move(msg));
                }));
            return subs;
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });
    co_await std::move(service_run);

    scheduler.stop();
    generators.stop_all();
    event_source.stop();
    co_return;
}

}
