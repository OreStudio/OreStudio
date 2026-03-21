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
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPointer>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

struct ServiceSamplesResult {
    bool success{false};
    QString error;
    std::vector<telemetry::domain::service_sample> samples;
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

        self->table_->setSortingEnabled(false);
        self->table_->setRowCount(static_cast<int>(result.samples.size()));

        int row = 0;
        for (const auto& sample : result.samples) {
            using namespace std::chrono;
            const auto age_secs = duration_cast<seconds>(
                now - sample.sampled_at).count();

            // RAG status
            QString status_text;
            QColor  status_color;
            if (age_secs < 30) {
                status_text  = tr("Green");
                status_color = badge_colors::online;
            } else if (age_secs < 90) {
                status_text  = tr("Amber");
                status_color = badge_colors::old;
            } else {
                status_text  = tr("Red");
                status_color = badge_colors::locked;
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
