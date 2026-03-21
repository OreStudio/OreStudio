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
#include "ores.qt/ComputeDashboardMdiWindow.hpp"

#include <chrono>
#include <set>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.compute/messaging/host_protocol.hpp"
#include "ores.compute/messaging/workunit_protocol.hpp"
#include "ores.compute/messaging/result_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

struct DashboardCounts {
    int total_hosts = 0;
    int idle_agents = 0;
    int total_workunits = 0;
    int total_results = 0;
    bool success = false;
    QString error;
};

}

ComputeDashboardMdiWindow::ComputeDashboardMdiWindow(
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      totalHostsLabel_(nullptr),
      onlineHostsLabel_(nullptr),
      pendingWorkunitLabel_(nullptr),
      activeWorkunitLabel_(nullptr),
      completedWorkunitLabel_(nullptr),
      totalResultsLabel_(nullptr),
      recentResultsLabel_(nullptr),
      autoRefreshTimer_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(10000);  // 10 seconds
    connect(autoRefreshTimer_, &QTimer::timeout, this,
            &ComputeDashboardMdiWindow::refresh);

    setupUi();
    refresh();
}

void ComputeDashboardMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(4, 4, 4, 4);
    mainLayout->setSpacing(4);

    setupToolbar();
    mainLayout->addWidget(toolbar_);

    // Main grid of summary boxes
    auto* grid = new QWidget(this);
    auto* gridLayout = new QGridLayout(grid);
    gridLayout->setSpacing(8);

    auto makeStatBox = [this](const QString& title, QLabel*& valueLabel) -> QGroupBox* {
        auto* box = new QGroupBox(title, this);
        auto* layout = new QVBoxLayout(box);
        valueLabel = new QLabel("—", box);
        valueLabel->setAlignment(Qt::AlignCenter);
        QFont f = valueLabel->font();
        f.setPointSize(f.pointSize() + 6);
        f.setBold(true);
        valueLabel->setFont(f);
        layout->addWidget(valueLabel);
        return box;
    };

    gridLayout->addWidget(makeStatBox(tr("Total Hosts"),     totalHostsLabel_),      0, 0);
    gridLayout->addWidget(makeStatBox(tr("Idle Agents"),     onlineHostsLabel_),     0, 1);
    gridLayout->addWidget(makeStatBox(tr("Total Workunits"), pendingWorkunitLabel_), 1, 0);
    gridLayout->addWidget(makeStatBox(tr("Active Workunits"),activeWorkunitLabel_),  1, 1);
    gridLayout->addWidget(makeStatBox(tr("Total Results"),   totalResultsLabel_),    2, 0);
    gridLayout->addWidget(makeStatBox(tr("Recent Results"),  recentResultsLabel_),   2, 1);

    mainLayout->addWidget(grid);
    mainLayout->addStretch();
}

void ComputeDashboardMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh dashboard statistics"));
    connect(refreshAction_, &QAction::triggered,
            this, &ComputeDashboardMdiWindow::refresh);

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Auto-Refresh"));
    autoRefreshAction_->setToolTip(tr("Toggle automatic refresh every 10 seconds"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &ComputeDashboardMdiWindow::onRefreshToggled);
}

void ComputeDashboardMdiWindow::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit statusChanged(tr("Not connected"));
        return;
    }

    refreshAction_->setEnabled(false);
    emit statusChanged(tr("Refreshing compute dashboard..."));
    loadCounts();
}

void ComputeDashboardMdiWindow::loadCounts() {
    QPointer<ComputeDashboardMdiWindow> self = this;

    QFuture<DashboardCounts> future =
        QtConcurrent::run([self]() -> DashboardCounts {
            if (!self || !self->clientManager_)
                return {};

            DashboardCounts counts;

            // Hosts: fetch all (grids are small) to compute total and idle count.
            // A host is "online" when its last_rpc_time is within the 5-minute
            // reaper threshold. A host is "idle" when it is online and has no
            // InProgress result currently assigned to it.
            std::set<std::string> online_host_ids;
            {
                compute::messaging::list_hosts_request req;
                req.limit = 1000;
                auto resp = self->clientManager_->
                    process_authenticated_request(std::move(req));
                if (resp) {
                    counts.total_hosts = resp->total_available_count;
                    const auto now = std::chrono::system_clock::now();
                    constexpr auto online_threshold = std::chrono::minutes(5);
                    for (const auto& h : resp->hosts) {
                        if (h.last_rpc_time ==
                                std::chrono::system_clock::time_point{})
                            continue;
                        if (now - h.last_rpc_time <= online_threshold)
                            online_host_ids.insert(boost::uuids::to_string(h.id));
                    }
                }
            }

            // Results: fetch all to find hosts currently busy (InProgress = 4).
            {
                compute::messaging::list_results_request req;
                req.limit = 10000;
                auto resp = self->clientManager_->
                    process_authenticated_request(std::move(req));
                if (resp) {
                    counts.total_results = resp->total_available_count;
                    std::set<std::string> busy_host_ids;
                    for (const auto& r : resp->results) {
                        if (r.server_state == 4)  // InProgress
                            busy_host_ids.insert(
                                boost::uuids::to_string(r.host_id));
                    }
                    for (const auto& id : online_host_ids) {
                        if (!busy_host_ids.count(id))
                            ++counts.idle_agents;
                    }
                }
            }

            // Workunits
            {
                compute::messaging::list_workunits_request req;
                req.limit = 1;
                auto resp = self->clientManager_->
                    process_authenticated_request(std::move(req));
                if (resp) {
                    counts.total_workunits = resp->total_available_count;
                }
            }

            counts.success = true;
            return counts;
        });

    auto* watcher = new QFutureWatcher<DashboardCounts>(this);
    connect(watcher, &QFutureWatcher<DashboardCounts>::finished, this,
            [self, watcher]() {
        auto counts = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->refreshAction_->setEnabled(true);

        if (!counts.success) {
            BOOST_LOG_SEV(lg(), error) << "Dashboard load failed";
            emit self->errorOccurred(tr("Failed to load dashboard statistics"));
            return;
        }

        self->updateCountLabel(self->totalHostsLabel_,     counts.total_hosts);
        self->updateCountLabel(self->onlineHostsLabel_,   counts.idle_agents);
        self->updateCountLabel(self->pendingWorkunitLabel_,counts.total_workunits);
        self->updateCountLabel(self->activeWorkunitLabel_, 0);  // Not yet tracked
        self->updateCountLabel(self->totalResultsLabel_,   counts.total_results);
        self->updateCountLabel(self->recentResultsLabel_,  0);  // Not yet tracked

        emit self->statusChanged(
            tr("Dashboard updated: %1 hosts, %2 idle, %3 workunits, %4 results")
                .arg(counts.total_hosts)
                .arg(counts.idle_agents)
                .arg(counts.total_workunits)
                .arg(counts.total_results));

        BOOST_LOG_SEV(lg(), debug) << "Dashboard: hosts=" << counts.total_hosts
                                   << " idle=" << counts.idle_agents
                                   << " workunits=" << counts.total_workunits
                                   << " results=" << counts.total_results;
    });
    watcher->setFuture(future);
}

void ComputeDashboardMdiWindow::updateCountLabel(QLabel* label, int value) {
    if (label)
        label->setText(QString::number(value));
}

void ComputeDashboardMdiWindow::onRefreshToggled(bool checked) {
    if (checked) {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh enabled for compute dashboard";
        autoRefreshTimer_->start();
    } else {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh disabled for compute dashboard";
        autoRefreshTimer_->stop();
    }
}

}
