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
#include "ores.qt/ServiceDashboardMdiWindow.hpp"

#include <chrono>
#include <QDateTime>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPointer>
#include <QHeaderView>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace compute = ores::compute;

namespace {

struct ServiceSamplesResult {
    bool success{false};
    QString error;
    std::vector<telemetry::domain::service_sample> samples;
    // Grid stats for wrapper node row (optional — absent if compute service unavailable)
    bool has_grid_stats{false};
    int total_hosts{0};
    int online_hosts{0};
    std::chrono::system_clock::time_point grid_sampled_at;
};

}

ServiceDashboardMdiWindow::ServiceDashboardMdiWindow(
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      table_(nullptr),
      autoRefreshTimer_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(30000);  // 30 seconds
    connect(autoRefreshTimer_, &QTimer::timeout, this,
            &ServiceDashboardMdiWindow::refresh);

    setupUi();
    refresh();
}

void ServiceDashboardMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(4, 4, 4, 4);
    mainLayout->setSpacing(4);

    setupToolbar();
    mainLayout->addWidget(toolbar_);

    table_ = new QTableWidget(this);
    table_->setColumnCount(5);
    table_->setHorizontalHeaderLabels({
        tr("Status"), tr("Service"), tr("Instance"), tr("Version"), tr("Last Seen")
    });
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setAlternatingRowColors(true);
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->verticalHeader()->setVisible(false);
    table_->setSortingEnabled(true);

    // Fixed width for status column
    table_->setColumnWidth(0, 70);

    mainLayout->addWidget(table_);
}

void ServiceDashboardMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh service status"));
    connect(refreshAction_, &QAction::triggered,
            this, &ServiceDashboardMdiWindow::refresh);

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Auto-Refresh"));
    autoRefreshAction_->setToolTip(tr("Toggle automatic refresh every 30 seconds"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &ServiceDashboardMdiWindow::onRefreshToggled);
}

void ServiceDashboardMdiWindow::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit statusChanged(tr("Not connected"));
        return;
    }

    refreshAction_->setEnabled(false);
    emit statusChanged(tr("Refreshing service dashboard..."));
    loadSamples();
}

void ServiceDashboardMdiWindow::loadSamples() {
    QPointer<ServiceDashboardMdiWindow> self = this;

    QFuture<ServiceSamplesResult> future =
        QtConcurrent::run([self]() -> ServiceSamplesResult {
            if (!self || !self->clientManager_)
                return {};

            auto resp = self->clientManager_->process_authenticated_request(
                telemetry::messaging::get_service_samples_request{});

            if (!resp || !resp->success) {
                ServiceSamplesResult r;
                r.error = QString::fromStdString(
                    resp ? resp->message : "Failed to contact telemetry service");
                return r;
            }

            ServiceSamplesResult r;
            r.success = true;
            r.samples = resp->samples;

            // Fetch grid stats for the wrapper node row (best-effort).
            try {
                auto grid = self->clientManager_->process_authenticated_request(
                    compute::messaging::get_grid_stats_request{});
                if (grid && grid->success) {
                    r.has_grid_stats = true;
                    r.total_hosts    = grid->total_hosts;
                    r.online_hosts   = grid->online_hosts;
                    const auto qdt = QDateTime::fromString(
                        QString::fromStdString(grid->sampled_at), Qt::ISODate);
                    if (qdt.isValid())
                        r.grid_sampled_at = std::chrono::system_clock::from_time_t(
                            qdt.toSecsSinceEpoch());
                }
            } catch (...) {
                // Compute service unavailable — omit the wrapper row.
            }

            return r;
        });

    auto* watcher = new QFutureWatcher<ServiceSamplesResult>(this);
    connect(watcher, &QFutureWatcher<ServiceSamplesResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->refreshAction_->setEnabled(true);

        if (!result.success) {
            const QString msg = result.error.isEmpty()
                ? tr("Failed to load service samples")
                : result.error;
            BOOST_LOG_SEV(lg(), error) << "Service dashboard load failed: "
                                       << msg.toStdString();
            emit self->errorOccurred(msg);
            return;
        }

        const auto now = std::chrono::system_clock::now();

        const int extra_rows = result.has_grid_stats ? 1 : 0;
        self->table_->setSortingEnabled(false);
        self->table_->setRowCount(
            static_cast<int>(result.samples.size()) + extra_rows);

        int row = 0;
        for (const auto& sample : result.samples) {
            using namespace std::chrono;
            const auto age_secs = duration_cast<seconds>(
                now - sample.sampled_at).count();

            // RAG status: Green=recent, Amber=slightly stale, Offline=stopped
            QString status_text;
            QColor  status_color;
            if (age_secs < 30) {
                status_text  = tr("Green");
                status_color = color_constants::level_info;
            } else if (age_secs < 120) {
                status_text  = tr("Amber");
                status_color = color_constants::level_warn;
            } else {
                status_text  = tr("Offline");
                status_color = color_constants::level_trace;
            }

            auto makeItem = [](const QString& text) {
                auto* item = new QTableWidgetItem(text);
                item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
                return item;
            };

            auto* statusItem = new QTableWidgetItem(status_text);
            statusItem->setTextAlignment(Qt::AlignCenter);
            statusItem->setForeground(status_color);
            QFont f = statusItem->font();
            f.setBold(true);
            statusItem->setFont(f);

            self->table_->setItem(row, 0, statusItem);
            self->table_->setItem(row, 1,
                makeItem(QString::fromStdString(sample.service_name)));
            self->table_->setItem(row, 2,
                makeItem(QString::fromStdString(sample.instance_id)));
            self->table_->setItem(row, 3,
                makeItem(QString::fromStdString(sample.version)));

            // Format last-seen as relative time
            QString last_seen;
            if (age_secs < 60)
                last_seen = tr("%1s ago").arg(age_secs);
            else if (age_secs < 3600)
                last_seen = tr("%1m ago").arg(age_secs / 60);
            else
                last_seen = tr("%1h ago").arg(age_secs / 3600);
            self->table_->setItem(row, 4, makeItem(last_seen));

            ++row;
        }

        // Wrapper nodes row
        if (result.has_grid_stats) {
            using namespace std::chrono;
            const auto age_secs = duration_cast<seconds>(
                now - result.grid_sampled_at).count();

            QString status_text;
            QColor  status_color;
            if (result.online_hosts > 0 && age_secs < 90) {
                status_text  = tr("Green");
                status_color = color_constants::level_info;
            } else if (result.total_hosts > 0) {
                status_text  = tr("Amber");
                status_color = color_constants::level_warn;
            } else {
                status_text  = tr("Red");
                status_color = color_constants::level_error;
            }

            auto makeItem = [](const QString& text) {
                auto* item = new QTableWidgetItem(text);
                item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
                return item;
            };

            auto* statusItem = new QTableWidgetItem(status_text);
            statusItem->setTextAlignment(Qt::AlignCenter);
            statusItem->setForeground(status_color);
            QFont f = statusItem->font();
            f.setBold(true);
            statusItem->setFont(f);

            const QString instance_text =
                tr("%1 node(s), %2 online")
                    .arg(result.total_hosts)
                    .arg(result.online_hosts);

            QString last_seen;
            if (age_secs < 60)
                last_seen = tr("%1s ago").arg(age_secs);
            else if (age_secs < 3600)
                last_seen = tr("%1m ago").arg(age_secs / 60);
            else
                last_seen = tr("%1h ago").arg(age_secs / 3600);

            self->table_->setItem(row, 0, statusItem);
            self->table_->setItem(row, 1, makeItem(tr("ores.compute.wrapper")));
            self->table_->setItem(row, 2, makeItem(instance_text));
            self->table_->setItem(row, 3, makeItem(tr("-")));
            self->table_->setItem(row, 4, makeItem(last_seen));
        }

        self->table_->setSortingEnabled(true);
        self->table_->resizeColumnsToContents();
        self->table_->horizontalHeader()->setStretchLastSection(true);

        BOOST_LOG_SEV(lg(), debug) << "Service dashboard: "
                                   << result.samples.size() << " services";
        emit self->statusChanged(
            tr("Service dashboard updated: %1 service instance(s)")
                .arg(result.samples.size()));
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onRefreshToggled(bool checked) {
    if (checked) {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh enabled for service dashboard";
        autoRefreshTimer_->start();
    } else {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh disabled for service dashboard";
        autoRefreshTimer_->stop();
    }
}

}
