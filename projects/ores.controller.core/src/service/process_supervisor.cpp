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
#include "ores.controller.core/service/process_supervisor.hpp"

#include <map>
#include <set>
#include <deque>
#include <fstream>
#include <sstream>
#include <chrono>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/process/v2/start_dir.hpp>
#include <boost/process/v2/stdio.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/name_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include "ores.platform/process/executable.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/service/exit_codes.hpp"
#include "ores.controller.core/repository/service_definition_repository.hpp"
#include "ores.controller.core/repository/service_dependency_repository.hpp"
#include "ores.controller.core/repository/service_instance_repository.hpp"
#include "ores.controller.core/repository/service_event_repository.hpp"

namespace ores::controller::service {

using namespace ores::logging;
namespace bp2 = boost::process::v2;

namespace {

// Simple whitespace tokeniser — splits args_template on spaces.
std::vector<std::string> tokenise(const std::string& s) {
    std::vector<std::string> tokens;
    std::istringstream iss(s);
    std::string tok;
    while (iss >> tok)
        tokens.push_back(tok);
    return tokens;
}

// Replace all occurrences of `from` with `to` inside `s`.
void replace_all(std::string& s, const std::string& from, const std::string& to) {
    std::string::size_type pos = 0;
    while ((pos = s.find(from, pos)) != std::string::npos) {
        s.replace(pos, from.size(), to);
        pos += to.size();
    }
}

constexpr std::chrono::seconds service_ready_timeout{60};
constexpr int log_tail_lines = 20;

// Returns the last `n` lines from `path`, joined by newlines.
// Seeks to the last 8 KB of the file to avoid reading large logs in full.
// Returns an empty string if the file does not exist or cannot be read.
std::string tail_log(const std::filesystem::path& path, int n) {
    std::ifstream f(path, std::ios::binary);
    if (!f) return {};

    // Seek to a window near the end large enough to contain n lines.
    constexpr std::streamoff window = 8192;
    f.seekg(0, std::ios::end);
    const auto file_size = f.tellg();
    const auto seek_pos = (file_size > window) ? (file_size - window) : 0;
    f.seekg(seek_pos);

    std::deque<std::string> lines;
    std::string line;
    while (std::getline(f, line)) {
        // Strip any trailing \r from Windows-style line endings.
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        lines.push_back(line);
        if (static_cast<int>(lines.size()) > n)
            lines.pop_front();
    }

    std::string result;
    for (const auto& l : lines) {
        if (!result.empty()) result += '\n';
        result += l;
    }
    return result;
}

} // namespace

process_supervisor::process_supervisor(boost::asio::io_context& ioc,
    std::filesystem::path bin_dir,
    ores::nats::config::nats_options nats,
    std::string log_level,
    ores::database::context db_ctx)
    : ioc_(ioc)
    , bin_dir_(std::move(bin_dir))
    , nats_(std::move(nats))
    , log_level_(std::move(log_level))
    , db_ctx_(std::move(db_ctx)) {}

std::filesystem::path process_supervisor::log_path_for(
    const std::string& service_name, int replica_index) const {
    const auto log_dir = bin_dir_ / ".." / "log";
    const auto filename = service_name + "." + std::to_string(replica_index) + ".log";
    return log_dir / filename;
}

std::filesystem::path process_supervisor::pid_path_for(
    const std::string& service_name, int replica_index) const {
    const auto run_dir = bin_dir_ / ".." / "run";
    // Replica 0 uses the bare service name (matches status-services.sh convention).
    // Higher replicas append the index so multiple replicas don't overwrite each other.
    const auto filename = replica_index == 0
        ? service_name + ".pid"
        : service_name + "." + std::to_string(replica_index) + ".pid";
    return run_dir / filename;
}

void process_supervisor::write_pid_file(const std::string& service_name,
    int replica_index, bp2::pid_type pid) const {
    const auto path = pid_path_for(service_name, replica_index);
    std::error_code ec;
    std::filesystem::create_directories(path.parent_path(), ec);
    std::ofstream f(path, std::ios::trunc);
    if (f)
        f << pid << '\n';
    else
        BOOST_LOG_SEV(lg(), warn) << "Could not write PID file: " << path;
}

void process_supervisor::remove_pid_file(const std::string& service_name,
    int replica_index) const {
    const auto path = pid_path_for(service_name, replica_index);
    std::error_code ec;
    std::filesystem::remove(path, ec);
}

std::vector<std::string> process_supervisor::build_args(
    const api::domain::service_definition& def, int replica_index) const {

    std::string tmpl = def.args_template.value_or(std::string(default_args_template));

    replace_all(tmpl, "{nats_url}",       nats_.url);
    replace_all(tmpl, "{nats_prefix}",    nats_.subject_prefix);
    replace_all(tmpl, "{tenant_id}",      nats_.subject_prefix);
    replace_all(tmpl, "{log_level}",      log_level_);
    replace_all(tmpl, "{log_dir}",        "../log");
    replace_all(tmpl, "{replica_index}",  std::to_string(replica_index));

    // {nats_tls_args}: derive per-service certs from controller's own cert directory.
    std::string tls_args;
    if (!nats_.tls_ca_cert.empty()) {
        const auto keys_dir =
            std::filesystem::path(nats_.tls_client_cert).parent_path();
        const auto cert = keys_dir / (def.service_name + ".crt");
        const auto key  = keys_dir / (def.service_name + ".key");
        tls_args = "--nats-tls-ca " + nats_.tls_ca_cert
                 + " --nats-tls-cert " + cert.string()
                 + " --nats-tls-key "  + key.string();
    }
    replace_all(tmpl, "{nats_tls_args}", tls_args);

    // {host_id}: stable UUID for compute wrapper nodes (hostname:replica_index).
    const std::string host_key =
        ores::platform::process::get_hostname() + ":" + std::to_string(replica_index);
    static const auto ns_uuid =
        boost::uuids::string_generator()("6ba7b810-9dad-11d1-80b4-00c04fd430c8");
    boost::uuids::name_generator name_gen(ns_uuid);
    replace_all(tmpl, "{host_id}",   boost::uuids::to_string(name_gen(host_key)));

    // {work_dir}: per-replica working directory for compute wrapper nodes.
    replace_all(tmpl, "{work_dir}",  "../run/wrappers/node_" + std::to_string(replica_index));

    return tokenise(tmpl);
}

boost::asio::awaitable<bool> process_supervisor::wait_for_log_ready(
    std::filesystem::path log_path, std::chrono::seconds timeout,
    std::uintmax_t start_offset) {

    const auto deadline = std::chrono::steady_clock::now() + timeout;
    auto executor = co_await boost::asio::this_coro::executor;

    while (std::chrono::steady_clock::now() < deadline) {
        // Check log file for "Service ready" — only in content written AFTER
        // start_offset so a "Service ready" from a previous run is ignored.
        if (std::filesystem::exists(log_path)) {
            std::ifstream f(log_path);
            if (start_offset > 0) {
                // If the log was truncated (new process overwrote it), fall
                // back to reading from the beginning.
                std::error_code ec;
                const auto cur_size = std::filesystem::file_size(log_path, ec);
                const auto seek_pos = (!ec && cur_size >= start_offset)
                    ? static_cast<std::streamoff>(start_offset) : 0;
                f.seekg(seek_pos);
            }
            std::string line;
            while (std::getline(f, line)) {
                if (line.find("Service ready") != std::string::npos)
                    co_return true;
            }
        }

        boost::asio::steady_timer t(executor);
        t.expires_after(std::chrono::milliseconds(500));
        co_await t.async_wait(boost::asio::use_awaitable);
    }

    co_return false;
}

boost::asio::awaitable<void> process_supervisor::start_all() {
    BOOST_LOG_SEV(lg(), info) << "Starting all enabled services (dependency-ordered)";

    repository::service_definition_repository def_repo;
    const auto defs = def_repo.read_latest(db_ctx_);

    repository::service_dependency_repository dep_repo;
    const auto deps = dep_repo.read_all(db_ctx_);

    // Build vertex index for enabled services.
    std::vector<std::string> names;
    std::map<std::string, std::size_t> name_to_v;
    for (const auto& def : defs) {
        if (!def.enabled) continue;
        name_to_v[def.service_name] = names.size();
        names.push_back(def.service_name);
    }

    // Directed graph: edge (A → B) means A must start before B.
    using Graph = boost::adjacency_list<boost::vecS, boost::vecS,
        boost::directedS>;
    Graph g(names.size());

    // Services that other services depend on must be waited for.
    std::set<std::string> has_dependents;

    for (const auto& [svc, dep] : deps) {
        auto it_svc = name_to_v.find(svc);
        auto it_dep = name_to_v.find(dep);
        if (it_svc == name_to_v.end() || it_dep == name_to_v.end()) continue;
        boost::add_edge(it_dep->second, it_svc->second, g);
        has_dependents.insert(dep);
    }

    // Topological sort — result is in reverse topological order.
    std::vector<std::size_t> order;
    try {
        boost::topological_sort(g, std::back_inserter(order));
    } catch (const boost::not_a_dag&) {
        BOOST_LOG_SEV(lg(), error)
            << "Cyclic dependency in service graph — falling back to unsorted start";
        for (std::size_t i = 0; i < names.size(); ++i)
            order.push_back(i);
    }

    // Iterate in reverse (topological order: dependencies first).
    for (auto it = order.rbegin(); it != order.rend(); ++it) {
        const auto& svc_name = names[*it];

        // Find the definition.
        const api::domain::service_definition* def_ptr = nullptr;
        for (const auto& d : defs) {
            if (d.service_name == svc_name) { def_ptr = &d; break; }
        }
        if (!def_ptr || !def_ptr->enabled) continue;

        // Snapshot the log file size BEFORE launching so wait_for_log_ready
        // only reads content written by the new process, ignoring any
        // "Service ready" left over from a previous run.
        std::uintmax_t log_start_offset = 0;
        if (has_dependents.count(svc_name)) {
            const auto log = log_path_for(svc_name, 0);
            std::error_code ec;
            const auto sz = std::filesystem::file_size(log, ec);
            if (!ec) log_start_offset = sz;
        }

        // Launch all replicas.
        for (int r = 0; r < def_ptr->desired_replicas; ++r) {
            const auto key = std::make_pair(svc_name, r);
            if (processes_.contains(key)) {
                BOOST_LOG_SEV(lg(), debug)
                    << "Skipping already-running: " << svc_name << "[" << r << "]";
                continue;
            }
            co_await do_launch(svc_name, r);
        }

        // Wait for readiness before launching dependents.
        if (has_dependents.count(svc_name)) {
            const auto log = log_path_for(svc_name, 0);
            BOOST_LOG_SEV(lg(), info)
                << "Waiting for " << svc_name << " ready (log: " << log << ")";
            const bool ready =
                co_await wait_for_log_ready(log, service_ready_timeout,
                    log_start_offset);
            if (ready)
                BOOST_LOG_SEV(lg(), info) << svc_name << " is ready";
            else
                BOOST_LOG_SEV(lg(), warn)
                    << svc_name << " did not become ready within "
                    << service_ready_timeout.count() << "s — proceeding anyway";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "All services started";
}

boost::asio::awaitable<void> process_supervisor::do_launch(
    std::string service_name, int replica_index) {

    const auto key = std::make_pair(service_name, replica_index);

    repository::service_definition_repository def_repo;
    const auto defs = def_repo.read_latest(db_ctx_);
    const api::domain::service_definition* def_ptr = nullptr;
    for (const auto& d : defs) {
        if (d.service_name == service_name) { def_ptr = &d; break; }
    }
    if (!def_ptr) {
        BOOST_LOG_SEV(lg(), error)
            << "No service definition found for: " << service_name;
        co_return;
    }
    const auto def = *def_ptr;

    const auto exe = bin_dir_ / def.binary_name;
    if (!std::filesystem::exists(exe)) {
        BOOST_LOG_SEV(lg(), error)
            << "Binary not found: " << exe << " for " << service_name;
        co_return;
    }

    const auto args = build_args(def, replica_index);

    BOOST_LOG_SEV(lg(), info)
        << "Launching " << service_name << "[" << replica_index << "]"
        << " binary=" << exe.filename().string();

    auto executor = co_await boost::asio::this_coro::executor;
    auto entry = std::make_shared<process_entry>();
    entry->def = def;
    entry->replica_index = replica_index;

    try {
        // cd into bin_dir then exec with a relative name (like the shell would
        // do with `cd "$BIN_DIR" && exec ./binary`), so that `ps` shows the
        // short name instead of the full absolute path.
        // Redirect stdout/stderr to /dev/null: child processes log via
        // Boost.Log to their own log files; any stray writes to std::cout or
        // std::cerr must not bleed through to the controller's terminal.
        bp2::filesystem::path bp_exe("./" + def.binary_name);
        bp2::filesystem::path bp_dir(bin_dir_.string());
        entry->proc.emplace(executor, bp_exe, args,
            bp2::process_start_dir(bp_dir),
            bp2::process_stdio{.in = nullptr, .out = nullptr, .err = nullptr});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to launch " << service_name << "[" << replica_index
            << "]: " << e.what();
        co_return;
    }

    const auto pid = entry->proc->id();
    BOOST_LOG_SEV(lg(), info)
        << "Launched " << service_name << "[" << replica_index
        << "] PID=" << pid;

    write_pid_file(service_name, replica_index, pid);

    // Update/create DB instance record with PID and phase=running.
    try {
        const auto now = std::chrono::system_clock::now();
        repository::service_instance_repository inst_repo;
        auto existing = inst_repo.read(db_ctx_, service_name, replica_index);
        if (existing) {
            existing->phase = "running";
            existing->pid = static_cast<int>(pid);
            existing->started_at = now;
            existing->stopped_at = std::nullopt;
            inst_repo.update_phase(db_ctx_, *existing);
        } else {
            api::domain::service_instance inst;
            inst.id = boost::uuids::random_generator()();
            inst.service_name = service_name;
            inst.replica_index = replica_index;
            inst.pid = static_cast<int>(pid);
            inst.phase = "running";
            inst.created_at = now;
            inst.started_at = now;
            inst_repo.insert(db_ctx_, inst);
        }

        repository::service_event_repository ev_repo;
        api::domain::service_event ev;
        ev.event_id = boost::uuids::random_generator()();
        ev.occurred_at = now;
        ev.service_name = service_name;
        ev.replica_index = replica_index;
        ev.event_type = "started";
        ev.message = "Process launched by supervisor, PID=" + std::to_string(pid);
        ev_repo.insert(db_ctx_, ev);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "DB update failed after launch of " << service_name
            << "[" << replica_index << "]: " << e.what();
    }

    entry->retry.on_start();
    processes_[key] = entry;

    // Monitor the process in the background.
    boost::asio::co_spawn(ioc_, monitor_process(entry), boost::asio::detached);
}

boost::asio::awaitable<void> process_supervisor::do_stop(
    std::string service_name, int replica_index) {

    const auto key = std::make_pair(service_name, replica_index);
    auto it = processes_.find(key);
    if (it == processes_.end()) {
        BOOST_LOG_SEV(lg(), warn)
            << "stop: no running process for "
            << service_name << "[" << replica_index << "]";
        co_return;
    }

    auto& entry = *it->second;
    entry.stop_requested = true;

    BOOST_LOG_SEV(lg(), info)
        << "Sending SIGTERM to " << service_name << "[" << replica_index
        << "] PID=" << entry.proc->id();
    boost::system::error_code ec;
    entry.proc->request_exit(ec);
    if (ec)
        BOOST_LOG_SEV(lg(), warn)
            << "request_exit failed: " << ec.message();
}

boost::asio::awaitable<void> process_supervisor::monitor_process(
    std::shared_ptr<process_entry> entry) {

    const auto& service_name = entry->def.service_name;
    const auto replica_index = entry->replica_index;
    const auto key = std::make_pair(service_name, replica_index);

    BOOST_LOG_SEV(lg(), debug)
        << "Monitoring " << service_name << "[" << replica_index
        << "] PID=" << entry->proc->id();

    boost::system::error_code ec;
    int exit_code = 0;
    try {
        exit_code = co_await entry->proc->async_wait(boost::asio::use_awaitable);
    } catch (const boost::system::system_error& e) {
        ec = e.code();
    }

    processes_.erase(key);
    remove_pid_file(service_name, replica_index);

    BOOST_LOG_SEV(lg(), info)
        << service_name << "[" << replica_index << "]"
        << " exited with code=" << exit_code
        << (entry->stop_requested ? " (stop requested)" : " (unexpected)");

    // Update DB.
    try {
        const auto now = std::chrono::system_clock::now();
        repository::service_instance_repository inst_repo;
        auto existing = inst_repo.read(db_ctx_, service_name, replica_index);
        if (existing) {
            existing->phase = entry->stop_requested ? "stopped" : "failed";
            existing->pid = std::nullopt;
            existing->stopped_at = now;
            inst_repo.update_phase(db_ctx_, *existing);
        }

        repository::service_event_repository ev_repo;
        api::domain::service_event ev;
        ev.event_id = boost::uuids::random_generator()();
        ev.occurred_at = now;
        ev.service_name = service_name;
        ev.replica_index = replica_index;
        ev.event_type = entry->stop_requested ? "stopped" : "exited";
        {
            using ores::service::service::exit_code_name;
            using ores::service::service::to_exit_code;
            ev.message = "Process exited, code=" + std::to_string(exit_code)
                + " (" + std::string(exit_code_name(to_exit_code(exit_code))) + ")"
                + (entry->stop_requested ? " [stop requested]" : " [unexpected]");
        }
        ev_repo.insert(db_ctx_, ev);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "DB update failed after exit of " << service_name
            << "[" << replica_index << "]: " << e.what();
    }

    if (entry->stop_requested)
        co_return;

    // Apply restart policy.
    const auto& policy = entry->def.restart_policy;
    const bool should_restart =
        (policy == "always") ||
        (policy == "on-failure" && exit_code != 0);

    if (!should_restart) {
        BOOST_LOG_SEV(lg(), info)
            << "Restart policy '" << policy
            << "' — not restarting " << service_name
            << "[" << replica_index << "]";
        co_return;
    }

    // Check if we have exceeded the configured restart limit before updating state.
    // If so, mark as permanently failed in the DB, but keep retrying so the service
    // recovers automatically once the underlying issue is resolved.
    const int max_restarts = entry->def.max_restart_count;
    if (entry->retry.exceeded(max_restarts)) {
        using ores::service::service::to_exit_code;
        using ores::service::service::exit_code_name;

        const auto ec = to_exit_code(exit_code);
        const auto ec_name = exit_code_name(ec);
        const auto log_path = log_path_for(service_name, replica_index);
        const auto log_snippet = tail_log(log_path, log_tail_lines);

        const std::string error_summary =
            std::string(ec_name) +
            " (failed after " + std::to_string(max_restarts) + " restart" +
            (max_restarts == 1 ? "" : "s") + ")" +
            "\n\n--- last " + std::to_string(log_tail_lines) +
            " log lines ---\n" + log_snippet;

        BOOST_LOG_SEV(lg(), error)
            << service_name << "[" << replica_index << "]"
            << " exceeded max_restart_count=" << max_restarts
            << " (" << ec_name << ") — marking failed, will keep retrying";

        try {
            const auto now = std::chrono::system_clock::now();
            repository::service_instance_repository inst_repo;
            auto inst = inst_repo.read(db_ctx_, service_name, replica_index);
            if (inst) {
                inst->phase = "failed";
                inst->last_error = error_summary;
                inst_repo.update_phase(db_ctx_, *inst);
            }

            repository::service_event_repository ev_repo;
            api::domain::service_event fail_ev;
            fail_ev.event_id = boost::uuids::random_generator()();
            fail_ev.occurred_at = now;
            fail_ev.service_name = service_name;
            fail_ev.replica_index = replica_index;
            fail_ev.event_type = "failed";
            fail_ev.message = error_summary;
            ev_repo.insert(db_ctx_, fail_ev);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to record failure state for " << service_name
                << "[" << replica_index << "]: " << e.what();
        }
        // Do NOT return — fall through and keep retrying at the capped interval.
    }

    // Delegate uptime check, counter management, and delay computation to the
    // strategy.  on_failure() resets the counter if uptime exceeded the threshold,
    // increments it, and returns the delay to wait before re-launching.
    const auto delay = entry->retry.on_failure();
    const int next_count = entry->retry.failure_count();

    BOOST_LOG_SEV(lg(), info)
        << "Restarting " << service_name << "[" << replica_index << "]"
        << " (attempt " << next_count << ")"
        << " after " << delay.count() << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer t(executor);
    t.expires_after(delay);
    co_await t.async_wait(boost::asio::use_awaitable);

    co_await do_launch(service_name, replica_index);

    // do_launch creates a new process_entry with a fresh retry_strategy (count=0).
    // Restore the accumulated count so the next failure continues the backoff curve
    // rather than starting over.
    auto new_it = processes_.find(key);
    if (new_it != processes_.end())
        new_it->second->retry.set_failure_count(next_count);

    // Persist the updated failure count to the DB so the dashboard reflects it.
    try {
        repository::service_instance_repository inst_repo;
        auto inst = inst_repo.read(db_ctx_, service_name, replica_index);
        if (inst) {
            inst->restart_count = next_count;
            inst_repo.update_phase(db_ctx_, *inst);
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to persist restart_count for " << service_name
            << "[" << replica_index << "]: " << e.what();
    }
}

boost::asio::awaitable<void> process_supervisor::stop_all() {
    BOOST_LOG_SEV(lg(), info)
        << "Stopping all supervised services (" << processes_.size() << " processes)";

    auto executor = co_await boost::asio::this_coro::executor;

    // Phase 1: SIGTERM — services handle this via signal_set for graceful drain.
    for (auto& [key, entry] : processes_) {
        entry->stop_requested = true;
        if (entry->proc) {
            boost::system::error_code ec;
            entry->proc->request_exit(ec);
            if (ec)
                BOOST_LOG_SEV(lg(), warn)
                    << "request_exit (SIGTERM) failed for " << key.first
                    << "[" << key.second << "]: " << ec.message();
            else
                BOOST_LOG_SEV(lg(), info)
                    << "Sent SIGTERM to " << key.first
                    << "[" << key.second << "] PID=" << entry->proc->id();
        }
    }

    // Phase 2: Poll for up to 5 s; well-behaved services exit here.
    constexpr auto sigterm_grace = std::chrono::seconds(5);
    const auto sigterm_deadline = std::chrono::steady_clock::now() + sigterm_grace;
    while (!processes_.empty() && std::chrono::steady_clock::now() < sigterm_deadline) {
        boost::asio::steady_timer t(executor);
        t.expires_after(std::chrono::milliseconds(200));
        co_await t.async_wait(boost::asio::use_awaitable);
    }

    // Phase 3: second graceful-shutdown request for any survivors.
    // Uses Boost.Process v2 request_exit() which is cross-platform
    // (SIGTERM on POSIX, WM_QUIT on Windows). Our services treat a
    // repeated graceful signal the same way as the first one.
    if (!processes_.empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << processes_.size()
            << " process(es) still running after first grace — sending second exit request";
        for (auto& [key, entry] : processes_) {
            if (entry->proc) {
                boost::system::error_code ec;
                entry->proc->request_exit(ec);
                BOOST_LOG_SEV(lg(), warn)
                    << "Sent second exit request to " << key.first
                    << "[" << key.second << "] PID=" << entry->proc->id();
            }
        }

        // Phase 4: Wait up to 5 s for the second request to take effect.
        constexpr auto sigquit_grace = std::chrono::seconds(5);
        const auto sigquit_deadline = std::chrono::steady_clock::now() + sigquit_grace;
        while (!processes_.empty() && std::chrono::steady_clock::now() < sigquit_deadline) {
            boost::asio::steady_timer t(executor);
            t.expires_after(std::chrono::milliseconds(200));
            co_await t.async_wait(boost::asio::use_awaitable);
        }
    }

    // Phase 5: SIGKILL any stragglers that outlived both grace periods.
    if (!processes_.empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << processes_.size()
            << " process(es) did not exit within grace period — sending SIGKILL";
        for (auto& [key, entry] : processes_) {
            if (entry->proc) {
                boost::system::error_code ec;
                entry->proc->terminate(ec);
                BOOST_LOG_SEV(lg(), warn)
                    << "SIGKILL sent to " << key.first
                    << "[" << key.second << "] PID=" << entry->proc->id();
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "All supervised services stopped.";
}

void process_supervisor::request_launch(
    const std::string& service_name, int replica_index) {

    boost::asio::co_spawn(ioc_,
        do_launch(service_name, replica_index),
        boost::asio::detached);
}

void process_supervisor::request_stop(
    const std::string& service_name, int replica_index) {

    boost::asio::co_spawn(ioc_,
        do_stop(service_name, replica_index),
        boost::asio::detached);
}

void process_supervisor::request_restart(
    const std::string& service_name, int replica_index) {

    boost::asio::co_spawn(ioc_,
        [this, service_name, replica_index]()
                -> boost::asio::awaitable<void> {
            co_await do_stop(service_name, replica_index);
            // Give the process a moment to exit before relaunching.
            auto executor = co_await boost::asio::this_coro::executor;
            boost::asio::steady_timer t(executor);
            t.expires_after(std::chrono::seconds(2));
            co_await t.async_wait(boost::asio::use_awaitable);
            co_await do_launch(service_name, replica_index);
        }(),
        boost::asio::detached);
}

} // namespace ores::controller::service
