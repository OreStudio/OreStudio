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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/WorkflowStepsWidget.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include <QApplication>
#include <QHeaderView>
#include <QPainter>
#include <QStyleOptionViewItem>
#include <QStyledItemDelegate>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;
namespace wf = ores::workflow::messaging;

namespace {

enum ItemRole {
    BadgeTagRole = Qt::UserRole,
    BadgeColorRole = Qt::UserRole + 1,
};

enum class Col { Index = 0, Name, Status, Warnings, StartedAt, CompletedAt, Error, Count };

class BadgeDelegate final : public QStyledItemDelegate {
public:
    explicit BadgeDelegate(QObject* parent = nullptr)
        : QStyledItemDelegate(parent) {}

    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        if (index.data(BadgeTagRole).toString() != QStringLiteral("badge")) {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }
        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        QApplication::style()->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        const QColor bg = index.data(BadgeColorRole).value<QColor>();
        const QColor fg = color_constants::level_text;

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);
        DelegatePaintUtils::draw_centered_badge(painter, opt.rect, text, bg, fg, badgeFont);
    }

    QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const override {
        QSize s = QStyledItemDelegate::sizeHint(option, index);
        if (index.data(BadgeTagRole).toString() == QStringLiteral("badge"))
            s = QSize(qMax(s.width(), 90), qMax(s.height(), 24));
        return s;
    }
};

QTableWidgetItem* make_badge_item(const QString& text, const QColor& bg) {
    auto* item = new QTableWidgetItem(text);
    item->setData(BadgeTagRole, QStringLiteral("badge"));
    item->setData(BadgeColorRole, bg);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QColor status_color(const QString& status) {
    if (status == QStringLiteral("completed"))
        return color_constants::level_info;
    if (status == QStringLiteral("completed_with_warnings"))
        return color_constants::level_warn;
    if (status == QStringLiteral("failed"))
        return color_constants::level_error;
    if (status == QStringLiteral("in_progress") || status == QStringLiteral("compensating"))
        return color_constants::level_warn;
    if (status == QStringLiteral("compensated"))
        return color_constants::level_debug;
    return color_constants::level_trace;
}

int count_issues(const std::vector<wf::workflow_step_summary>& steps, int row) {
    const auto& log = steps[static_cast<std::size_t>(row)].log;
    return static_cast<int>(std::count_if(log.begin(), log.end(), [](const wf::step_log_entry& e) {
        return e.level == wf::step_log_level::warn || e.level == wf::step_log_level::error;
    }));
}

constexpr int kRefreshIntervalMs = 3000;

} // namespace

WorkflowStepsWidget::WorkflowStepsWidget(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , headerLabel_(nullptr)
    , stepsTable_(nullptr)
    , refreshTimer_(new QTimer(this))
    , watcher_(new QFutureWatcher<FetchResult>(this)) {

    setupUi();

    refreshTimer_->setInterval(kRefreshIntervalMs);
    connect(refreshTimer_, &QTimer::timeout, this, &WorkflowStepsWidget::refresh);

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &WorkflowStepsWidget::onFetchFinished);
}

void WorkflowStepsWidget::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    headerLabel_ = new QLabel(tr("Waiting for step data…"), this);
    layout->addWidget(headerLabel_);

    stepsTable_ = new QTableWidget(0, static_cast<int>(Col::Count), this);
    stepsTable_->setHorizontalHeaderLabels({tr("#"),
                                            tr("Name"),
                                            tr("Status"),
                                            tr("Warnings"),
                                            tr("Started At"),
                                            tr("Completed At"),
                                            tr("Error")});
    stepsTable_->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
    stepsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    stepsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    stepsTable_->setAlternatingRowColors(true);
    stepsTable_->setItemDelegate(new BadgeDelegate(stepsTable_));
    layout->addWidget(stepsTable_);

    connect(stepsTable_->selectionModel(),
            &QItemSelectionModel::currentRowChanged,
            this,
            [this](const QModelIndex& current, const QModelIndex&) {
                const int row = current.row();
                if (row >= 0 && row < static_cast<int>(currentSteps_.size()))
                    emit stepSelected(currentSteps_[static_cast<std::size_t>(row)]);
            });
}

void WorkflowStepsWidget::setInstance(const QUuid& instanceId) {
    instanceId_ = instanceId;
    terminalReached_ = false;
    preSeedCount_ = 0;
    stepsTable_->setRowCount(0);

    if (instanceId_.isNull()) {
        refreshTimer_->stop();
        headerLabel_->setText(tr("No workflow instance selected."));
        return;
    }

    headerLabel_->setText(tr("Loading…"));
    refresh();
    refreshTimer_->start();
}

void WorkflowStepsWidget::refresh() {
    if (instanceId_.isNull() || terminalReached_)
        return;
    if (!clientManager_ || !clientManager_->isConnected())
        return;
    if (watcher_->isRunning())
        return; // previous fetch still in flight

    QPointer<WorkflowStepsWidget> self = this;
    const std::string id = instanceId_.toString(QUuid::WithoutBraces).toStdString();

    watcher_->setFuture(QtConcurrent::run([self, id]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {};
        try {
            wf::get_workflow_steps_request req;
            req.workflow_instance_id = id;
            auto resp = self->clientManager_->process_authenticated_request(req);
            if (!resp)
                return {false, QString::fromStdString(resp.error()), {}};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message), {}};
            return {true, {}, std::move(resp->steps)};
        } catch (const std::exception& e) {
            return {false, QString::fromUtf8(e.what()), {}};
        } catch (...) {
            return {false, QStringLiteral("Unknown error loading steps"), {}};
        }
    }));
}

void WorkflowStepsWidget::setMaxVisibleSteps(int n) {
    maxVisibleSteps_ = n;
}

void WorkflowStepsWidget::preSeed(int count) {
    if (count <= 0)
        return;
    preSeedCount_ = count;
    stepsTable_->setRowCount(count);
    for (int row = 0; row < count; ++row) {
        stepsTable_->setItem(row, static_cast<int>(Col::Index), make_item(QString::number(row)));
        stepsTable_->setItem(row, static_cast<int>(Col::Name), make_item(QStringLiteral("—")));
        stepsTable_->setItem(
            row,
            static_cast<int>(Col::Status),
            make_badge_item(QStringLiteral("pending"), color_constants::level_trace));
        stepsTable_->setItem(row, static_cast<int>(Col::Warnings), make_item(QStringLiteral("—")));
        stepsTable_->setItem(row, static_cast<int>(Col::StartedAt), make_item(QStringLiteral("—")));
        stepsTable_->setItem(
            row, static_cast<int>(Col::CompletedAt), make_item(QStringLiteral("—")));
        stepsTable_->setItem(row, static_cast<int>(Col::Error), make_item(QString{}));
    }
    headerLabel_->setText(tr("%1 step(s) queued").arg(count));
}

void WorkflowStepsWidget::onFetchFinished() {
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn) << "Step fetch failed: " << result.error.toStdString();
        headerLabel_->setText(tr("Error loading steps: %1").arg(result.error));
        return;
    }

    populateSteps(result.steps);
}

void WorkflowStepsWidget::populateSteps(const std::vector<wf::workflow_step_summary>& steps) {

    auto visible = steps;

    // Apply maxVisibleSteps limit (take the last N steps).
    if (maxVisibleSteps_ > 0 && static_cast<int>(visible.size()) > maxVisibleSteps_) {
        visible.erase(visible.begin(),
                      visible.begin() + static_cast<std::ptrdiff_t>(visible.size()) -
                          maxVisibleSteps_);
    }

    currentSteps_ = visible;

    // When pre-seeded, update rows in-place by step_index to avoid row
    // insertions (which cause layout jumps). Without pre-seeding, fall back
    // to setting the row count and iterating in order.
    const bool inPlace = (preSeedCount_ > 0);
    if (!inPlace)
        stepsTable_->setRowCount(static_cast<int>(visible.size()));

    for (int i = 0; i < static_cast<int>(visible.size()); ++i) {
        const auto& step = visible[static_cast<std::size_t>(i)];
        const int row = inPlace ? step.step_index : i;
        if (row < 0 || row >= stepsTable_->rowCount())
            continue;

        stepsTable_->setItem(
            row, static_cast<int>(Col::Index), make_item(QString::number(step.step_index)));
        stepsTable_->setItem(
            row, static_cast<int>(Col::Name), make_item(QString::fromStdString(step.name)));

        const QString status = QString::fromStdString(step.status);
        stepsTable_->setItem(
            row, static_cast<int>(Col::Status), make_badge_item(status, status_color(status)));

        const int issues = count_issues(visible, i);
        if (issues > 0) {
            const bool has_errors =
                std::any_of(step.log.begin(), step.log.end(), [](const wf::step_log_entry& e) {
                    return e.level == wf::step_log_level::error;
                });
            const QColor badge_col =
                has_errors ? color_constants::level_error : color_constants::level_warn;
            const QString badge_text = tr("%1 issue(s)").arg(issues);
            stepsTable_->setItem(
                row, static_cast<int>(Col::Warnings), make_badge_item(badge_text, badge_col));
        } else {
            stepsTable_->setItem(
                row, static_cast<int>(Col::Warnings), make_item(QStringLiteral("—")));
        }

        stepsTable_->setItem(row,
                             static_cast<int>(Col::StartedAt),
                             make_item(step.started_at ? QString::fromStdString(*step.started_at) :
                                                         QStringLiteral("—")));
        stepsTable_->setItem(row,
                             static_cast<int>(Col::CompletedAt),
                             make_item(step.completed_at ?
                                           QString::fromStdString(*step.completed_at) :
                                           QStringLiteral("—")));
        stepsTable_->setItem(
            row, static_cast<int>(Col::Error), make_item(QString::fromStdString(step.error)));
    }

    stepsTable_->resizeColumnsToContents();
    stepsTable_->horizontalHeader()->setStretchLastSection(true);

    // Update header label: show running step or terminal state.
    const int total = static_cast<int>(steps.size());
    int runningIdx = -1;
    int failedIdx = -1;
    int completedCount = 0;
    int warnedCount = 0;
    QString failedError;

    for (const auto& s : steps) {
        const auto st = QString::fromStdString(s.status);
        if (st == QStringLiteral("in_progress") || st == QStringLiteral("compensating"))
            runningIdx = s.step_index;
        else if (st == QStringLiteral("failed")) {
            failedIdx = s.step_index;
            failedError = QString::fromStdString(s.error);
        } else if (st == QStringLiteral("completed_with_warnings")) {
            ++completedCount;
            ++warnedCount;
        } else if (st == QStringLiteral("completed")) {
            ++completedCount;
        }
    }

    if (failedIdx >= 0) {
        headerLabel_->setText(tr("Failed at step %1 of %2").arg(failedIdx + 1).arg(total));
        if (!terminalReached_) {
            terminalReached_ = true;
            refreshTimer_->stop();
            emit stepFailed(failedIdx, failedError);
            emit instanceReachedTerminalState(false);
        }
    } else if (total > 0 && completedCount == total) {
        if (warnedCount > 0) {
            headerLabel_->setText(
                tr("All %1 step(s) completed (%2 with warnings)").arg(total).arg(warnedCount));
        } else {
            headerLabel_->setText(tr("All %1 step(s) completed").arg(total));
        }
        if (!terminalReached_) {
            terminalReached_ = true;
            refreshTimer_->stop();
            emit instanceReachedTerminalState(true);
        }
    } else if (runningIdx >= 0) {
        headerLabel_->setText(tr("Step %1 of %2 in progress…").arg(runningIdx + 1).arg(total));
    } else if (total > 0) {
        headerLabel_->setText(tr("%1 step(s) pending").arg(total));
    } else {
        headerLabel_->setText(tr("Waiting for steps…"));
    }
}

} // namespace ores::qt
