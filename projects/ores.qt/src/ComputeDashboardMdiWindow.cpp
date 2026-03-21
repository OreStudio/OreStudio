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

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.compute/messaging/telemetry_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

struct GridStats {
    bool success{false};
    QString error;

    int total_hosts{0};
    int idle_hosts{0};
    int total_workunits{0};
    int results_in_progress{0};
    int results_done{0};
    int outcomes_success{0};
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
      idleHostsLabel_(nullptr),
      totalWorkunitLabel_(nullptr),
      inProgressLabel_(nullptr),
      completedLabel_(nullptr),
      successfulLabel_(nullptr),
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

    gridLayout->addWidget(makeStatBox(tr("Total Hosts"),       totalHostsLabel_),   0, 0);
    gridLayout->addWidget(makeStatBox(tr("Idle Hosts"),        idleHostsLabel_),    0, 1);
    gridLayout->addWidget(makeStatBox(tr("Total Workunits"),   totalWorkunitLabel_),1, 0);
    gridLayout->addWidget(makeStatBox(tr("In Progress"),       inProgressLabel_),   1, 1);
    gridLayout->addWidget(makeStatBox(tr("Completed"),         completedLabel_),    2, 0);
    gridLayout->addWidget(makeStatBox(tr("Successful (24h)"),  successfulLabel_),   2, 1);

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
    loadStats();
}

void ComputeDashboardMdiWindow::loadStats() {
    QPointer<ComputeDashboardMdiWindow> self = this;

    QFuture<GridStats> future =
        QtConcurrent::run([self]() -> GridStats {
            if (!self || !self->clientManager_)
                return {};

            auto resp = self->clientManager_->process_authenticated_request(
                compute::messaging::get_grid_stats_request{});

            if (!resp || !resp->success) {
                GridStats r;
                r.error = QString::fromStdString(
                    resp ? resp->message : "Failed to contact compute service");
                return r;
            }

            GridStats r;
            r.success           = true;
            r.total_hosts       = resp->total_hosts;
            r.idle_hosts        = resp->idle_hosts;
            r.total_workunits   = resp->total_workunits;
            r.results_in_progress = resp->results_in_progress;
            r.results_done      = resp->results_done;
            r.outcomes_success  = resp->outcomes_success;
            return r;
        });

    auto* watcher = new QFutureWatcher<GridStats>(this);
    connect(watcher, &QFutureWatcher<GridStats>::finished, this,
            [self, watcher]() {
        const auto stats = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->refreshAction_->setEnabled(true);

        if (!stats.success) {
            const QString msg = stats.error.isEmpty()
                ? tr("Failed to load dashboard statistics")
                : stats.error;
            BOOST_LOG_SEV(lg(), error) << "Dashboard load failed: "
                                       << msg.toStdString();
            emit self->errorOccurred(msg);
            return;
        }

        self->updateCountLabel(self->totalHostsLabel_,    stats.total_hosts);
        self->updateCountLabel(self->idleHostsLabel_,     stats.idle_hosts);
        self->updateCountLabel(self->totalWorkunitLabel_, stats.total_workunits);
        self->updateCountLabel(self->inProgressLabel_,    stats.results_in_progress);
        self->updateCountLabel(self->completedLabel_,     stats.results_done);
        self->updateCountLabel(self->successfulLabel_,    stats.outcomes_success);

        emit self->statusChanged(
            tr("Dashboard updated: %1 hosts, %2 idle, %3 workunits, %4 in progress")
                .arg(stats.total_hosts)
                .arg(stats.idle_hosts)
                .arg(stats.total_workunits)
                .arg(stats.results_in_progress));

        BOOST_LOG_SEV(lg(), debug) << "Dashboard: hosts=" << stats.total_hosts
                                   << " idle=" << stats.idle_hosts
                                   << " workunits=" << stats.total_workunits
                                   << " in_progress=" << stats.results_in_progress
                                   << " done=" << stats.results_done
                                   << " successful_24h=" << stats.outcomes_success;
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
