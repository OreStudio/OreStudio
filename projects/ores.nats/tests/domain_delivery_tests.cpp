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
#include "ores.nats/domain/delivery.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <type_traits>
#include <utility>
#include <vector>

namespace {

using ores::nats::delivery;
using namespace std::chrono_literals;

const auto tags("[nats][delivery]");

/**
 * @brief What a settler was asked, in order, including its own destruction.
 */
class journal final {
public:
    void add(std::string event) {
        const std::lock_guard lock(mutex_);
        events_.push_back(std::move(event));
    }

    [[nodiscard]] std::vector<std::string> events() const {
        const std::lock_guard lock(mutex_);
        return events_;
    }

    /// The events with the progress reports left out.
    [[nodiscard]] std::vector<std::string> answers() const {
        std::vector<std::string> out;
        for (auto& e : events())
            if (e != "in_progress")
                out.push_back(e);
        return out;
    }

private:
    mutable std::mutex mutex_;
    std::vector<std::string> events_;
};

class recording_settler final : public ores::nats::delivery_settler {
public:
    explicit recording_settler(std::shared_ptr<journal> j)
        : journal_(std::move(j)) {}

    ~recording_settler() override {
        journal_->add("freed");
    }

    recording_settler(const recording_settler&) = delete;
    recording_settler& operator=(const recording_settler&) = delete;

    void ack() noexcept override {
        journal_->add("ack");
    }
    void nak() noexcept override {
        journal_->add("nak");
    }
    void nak_after(std::chrono::milliseconds delay) noexcept override {
        journal_->add("nak_after " + std::to_string(delay.count()));
    }
    void term() noexcept override {
        journal_->add("term");
    }
    void in_progress() noexcept override {
        journal_->add("in_progress");
    }

private:
    std::shared_ptr<journal> journal_;
};

delivery make_delivery(const std::shared_ptr<journal>& j, std::string subject = "test.subject") {
    ores::nats::message msg;
    msg.subject = std::move(subject);
    return {std::move(msg), std::make_unique<recording_settler>(j)};
}

using answers = std::vector<std::string>;

}

TEST_CASE("a_delivery_dropped_without_a_settlement_rolls_back", tags) {
    auto j = std::make_shared<journal>();
    {
        auto d = make_delivery(j);
    }
    CHECK(j->events() == answers{"nak", "freed"});
}

TEST_CASE("each_settlement_sends_its_own_answer_once", tags) {
    auto j = std::make_shared<journal>();
    make_delivery(j).commit();
    make_delivery(j).rollback();
    make_delivery(j).rollback_after(250ms);
    make_delivery(j).reject();
    CHECK(j->events() ==
          answers{"ack", "freed", "nak", "freed", "nak_after 250", "freed", "term", "freed"});
}

TEST_CASE("a_settled_delivery_answers_nothing_more_when_it_is_dropped", tags) {
    auto j = std::make_shared<journal>();
    {
        auto d = make_delivery(j);
        std::move(d).commit();
    }
    CHECK(j->events() == answers{"ack", "freed"});
}

TEST_CASE("a_moved_delivery_is_settled_only_by_its_new_holder", tags) {
    auto j = std::make_shared<journal>();
    {
        auto first = make_delivery(j, "kept");
        auto second = std::move(first);
        CHECK(second.msg().subject == "kept");
        std::move(second).commit();
    }
    CHECK(j->events() == answers{"ack", "freed"});
}

TEST_CASE("a_delivery_assigned_over_an_open_one_rolls_the_open_one_back", tags) {
    auto replaced = std::make_shared<journal>();
    auto kept = std::make_shared<journal>();
    auto d = make_delivery(replaced);
    d = make_delivery(kept);
    CHECK(replaced->events() == answers{"nak", "freed"});
    std::move(d).commit();
    CHECK(kept->events() == answers{"ack", "freed"});
}

TEST_CASE("a_delivery_cannot_be_copied", tags) {
    STATIC_CHECK_FALSE(std::is_copy_constructible_v<delivery>);
    STATIC_CHECK_FALSE(std::is_copy_assignable_v<delivery>);
    STATIC_CHECK(std::is_nothrow_move_constructible_v<delivery>);
}

TEST_CASE("a_kept_alive_delivery_reports_progress_until_it_is_settled", tags) {
    auto j = std::make_shared<journal>();
    std::atomic<int> ticks{0};
    auto d = make_delivery(j);
    d.keep_alive(5ms, [&ticks] {
        ticks.fetch_add(1);
        return true;
    });
    while (ticks.load() < 3)
        std::this_thread::sleep_for(1ms);
    std::move(d).commit();
    const auto after = ticks.load();
    std::this_thread::sleep_for(30ms);

    CHECK(ticks.load() == after);
    const auto events = j->events();
    REQUIRE(events.size() >= 5);
    CHECK(events[events.size() - 2] == "ack");
    CHECK(events.back() == "freed");
    CHECK(j->answers() == answers{"ack", "freed"});
}

TEST_CASE("a_keep_alive_tick_that_returns_false_ends_the_reports", tags) {
    auto j = std::make_shared<journal>();
    std::atomic<int> ticks{0};
    auto d = make_delivery(j);
    d.keep_alive(5ms, [&ticks] {
        ticks.fetch_add(1);
        return false;
    });
    std::this_thread::sleep_for(50ms);
    CHECK(ticks.load() == 1);
    std::move(d).rollback();
    CHECK(j->events() == answers{"in_progress", "nak", "freed"});
}

TEST_CASE("a_kept_alive_delivery_moved_to_another_thread_keeps_reporting", tags) {
    auto j = std::make_shared<journal>();
    std::atomic<int> ticks{0};
    auto d = make_delivery(j);
    d.keep_alive(5ms, [&ticks] {
        ticks.fetch_add(1);
        return true;
    });
    std::thread holder([kept = std::move(d), &ticks]() mutable {
        while (ticks.load() < 3)
            std::this_thread::sleep_for(1ms);
        std::move(kept).commit();
    });
    holder.join();
    CHECK(j->answers() == answers{"ack", "freed"});
}
