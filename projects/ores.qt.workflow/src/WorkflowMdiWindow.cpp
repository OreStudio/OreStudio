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
#include "ores.qt/WorkflowMdiWindow.hpp"

#include <algorithm>
#include <QCloseEvent>
#include <QDialog>
#include <QDialogButtonBox>
#include <QFontDatabase>
#include <QFrame>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QLabel>
#include <QMessageBox>
#include <QPainter>
#include <QPlainTextEdit>
#include <QSplitter>
#include <QSortFilterProxyModel>
#include <QStyledItemDelegate>
#include <QStyleOptionViewItem>
#include <QVBoxLayout>
#include <QApplication>
#include <QHeaderView>
#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace wf = ores::workflow::messaging;

// ─────────────────────────────────────────────────────────────────────────────
// Local constants
// ─────────────────────────────────────────────────────────────────────────────

namespace {

constexpr std::string_view kWorkflowChangedEvent =
    "ores.workflow.workflow_instance_changed";

constexpr auto kSettingsGroup = "WorkflowMdiWindow";

// Custom item roles
enum ItemRole {
    BadgeTagRole   = Qt::UserRole,
    BadgeColorRole = Qt::UserRole + 1,
    InstanceIdRole = Qt::UserRole + 2,
    ErrorDetailRole = Qt::UserRole + 3,
    CorrIdRole     = Qt::UserRole + 4,
};

// Execution list table columns
enum class Col {
    Status = 0,
    Type,
    Steps,
    CreatedBy,
    CreatedAt,
    CorrelationId,
    Count
};

// Steps detail table columns
enum class SCol {
    Index = 0,
    Name,
    Status,
    StartedAt,
    CompletedAt,
    Error,
    Count
};

// Dashboard failures table columns
enum class FCol {
    Type = 0,
    CreatedAt,
    Error,
    Count
};

// ─────────────────────────────────────────────────────────────────────────────
// Badge delegate
// ─────────────────────────────────────────────────────────────────────────────

class BadgeDelegate final : public QStyledItemDelegate {
public:
    explicit BadgeDelegate(QObject* parent = nullptr)
        : QStyledItemDelegate(parent) {}

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        if (index.data(BadgeTagRole).toString() != QStringLiteral("badge")) {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }

        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        QApplication::style()->drawPrimitive(
            QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        const QColor  bg   = index.data(BadgeColorRole).value<QColor>();
        const QColor  fg   = color_constants::level_text;

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        DelegatePaintUtils::draw_centered_badge(
            painter, opt.rect, text, bg, fg, badgeFont);
    }

    QSize sizeHint(const QStyleOptionViewItem& option,
                   const QModelIndex& index) const override {
        QSize s = QStyledItemDelegate::sizeHint(option, index);
        if (index.data(BadgeTagRole).toString() == QStringLiteral("badge"))
            s = QSize(qMax(s.width(), 90), qMax(s.height(), 24));
        return s;
    }
};

// ─────────────────────────────────────────────────────────────────────────────
// Helper factories
// ─────────────────────────────────────────────────────────────────────────────

QTableWidgetItem* make_badge_item(const QString& text, const QColor& bg) {
    auto* item = new QTableWidgetItem(text);
    item->setData(BadgeTagRole,   QStringLiteral("badge"));
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

// Map internal state name to user-visible display label.
// "compensating" and "compensated" are failure states from the user's perspective.
QString display_status(const QString& status) {
    if (status == QStringLiteral("compensating") ||
        status == QStringLiteral("compensated"))
        return QStringLiteral("Failed");
    return status;
}

QColor status_color(const QString& status) {
    if (status == QStringLiteral("completed"))
        return color_constants::level_info;
    if (status == QStringLiteral("failed") ||
        status == QStringLiteral("compensating") ||
        status == QStringLiteral("compensated"))
        return color_constants::level_error;
    if (status == QStringLiteral("in_progress"))
        return color_constants::level_warn;
    return color_constants::level_trace;
}

bool is_failed(const QString& status) {
    return status == QStringLiteral("failed") ||
           status == QStringLiteral("compensating") ||
           status == QStringLiteral("compensated");
}

} // namespace

// ─────────────────────────────────────────────────────────────────────────────
// WorkflowMdiWindow
// ─────────────────────────────────────────────────────────────────────────────

WorkflowMdiWindow::WorkflowMdiWindow(ClientManager* clientManager,
                                     QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      autoRefreshTimer_(nullptr),
      tabs_(nullptr),
      activeCountLabel_(nullptr),
      failedCountLabel_(nullptr),
      failuresTable_(nullptr),
      searchEdit_(nullptr),
      statusFilter_(nullptr),
      instanceTable_(nullptr),
      splitter_(nullptr),
      stepsGroup_(nullptr),
      stepsTable_(nullptr),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      stepsWatcher_(new QFutureWatcher<StepsFetchResult>(this)) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(60'000);

    connect(autoRefreshTimer_, &QTimer::timeout,
            this, &WorkflowMdiWindow::reload);
    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &WorkflowMdiWindow::onFetchFinished);
    connect(stepsWatcher_, &QFutureWatcher<StepsFetchResult>::finished,
            this, &WorkflowMdiWindow::onStepsFetchFinished);

    setupUi();
    setupEventSubscriptions();

    // Restore persisted geometry.
    UiPersistence::restoreSize(
        QLatin1String(kSettingsGroup), {900, 600});
    UiPersistence::restoreSplitter(
        QLatin1String(kSettingsGroup), splitter_);

    reload();
}

void WorkflowMdiWindow::closeEvent(QCloseEvent* event) {
    UiPersistence::saveSize(QLatin1String(kSettingsGroup), this);
    UiPersistence::saveSplitter(QLatin1String(kSettingsGroup), splitter_);
    EntityListMdiWindow::closeEvent(event);
}

void WorkflowMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTabs();
    layout->addWidget(tabs_);
}

void WorkflowMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setIconSize(QSize(16, 16));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh workflow list"));
    connect(refreshAction_, &QAction::triggered,
            this, &WorkflowMdiWindow::reload);

    toolbar_->addSeparator();

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor),
        tr("Auto Refresh"));
    autoRefreshAction_->setToolTip(
        tr("Enable automatic refresh; click again to disable"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &WorkflowMdiWindow::onRefreshToggled);

    initializeStaleIndicator(refreshAction_,
        IconUtils::iconPath(Icon::ArrowSync));
}

void WorkflowMdiWindow::setupTabs() {
    tabs_ = new QTabWidget(this);

    auto* execTab = new QWidget;
    setupExecutionListTab(execTab);
    tabs_->addTab(execTab, tr("Execution List"));

    auto* dashTab = new QWidget;
    setupDashboardTab(dashTab);
    tabs_->addTab(dashTab, tr("Dashboard"));
}

void WorkflowMdiWindow::setupDashboardTab(QWidget* tab) {
    auto* layout = new QVBoxLayout(tab);

    // ── Count cards ──────────────────────────────────────────────────────────
    auto* cardsLayout = new QHBoxLayout;

    auto makeCard = [this](const QString& label, QLabel*& countOut) {
        auto* frame = new QFrame(this);
        frame->setFrameShape(QFrame::StyledPanel);
        auto* fl = new QVBoxLayout(frame);
        auto* title = new QLabel(label, frame);
        title->setAlignment(Qt::AlignCenter);
        countOut = new QLabel(QStringLiteral("0"), frame);
        countOut->setAlignment(Qt::AlignCenter);
        QFont f = countOut->font();
        f.setPointSize(f.pointSize() + 8);
        f.setBold(true);
        countOut->setFont(f);
        fl->addWidget(title);
        fl->addWidget(countOut);
        return frame;
    };

    cardsLayout->addWidget(makeCard(tr("Active"), activeCountLabel_));
    cardsLayout->addWidget(makeCard(tr("Failed"), failedCountLabel_));
    layout->addLayout(cardsLayout);

    // ── Recent failures ──────────────────────────────────────────────────────
    layout->addWidget(new QLabel(tr("Recent Failures:"), tab));

    failuresTable_ = new QTableWidget(0, static_cast<int>(FCol::Count), tab);
    failuresTable_->setHorizontalHeaderLabels(
        {tr("Type"), tr("Created At"), tr("Error")});
    failuresTable_->horizontalHeader()->setStretchLastSection(true);
    failuresTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    failuresTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    failuresTable_->setAlternatingRowColors(true);
    layout->addWidget(failuresTable_);
}

void WorkflowMdiWindow::setupExecutionListTab(QWidget* tab) {
    auto* layout = new QVBoxLayout(tab);
    layout->setContentsMargins(0, 0, 0, 0);

    // ── Filter bar ───────────────────────────────────────────────────────────
    auto* filterLayout = new QHBoxLayout;
    filterLayout->setContentsMargins(4, 4, 4, 0);
    searchEdit_ = new QLineEdit(tab);
    searchEdit_->setPlaceholderText(tr("Search by type or correlation ID…"));
    filterLayout->addWidget(searchEdit_);

    statusFilter_ = new QComboBox(tab);
    statusFilter_->addItem(tr("All Statuses"), QStringLiteral(""));
    statusFilter_->addItem(tr("In Progress"),   QStringLiteral("in_progress"));
    statusFilter_->addItem(tr("Completed"),      QStringLiteral("completed"));
    statusFilter_->addItem(tr("Failed"),         QStringLiteral("failed"));
    filterLayout->addWidget(statusFilter_);
    layout->addLayout(filterLayout);

    // ── Instance table (top of splitter) ─────────────────────────────────────
    instanceTable_ = new QTableWidget(0, static_cast<int>(Col::Count), tab);
    instanceTable_->setHorizontalHeaderLabels(
        {tr("Status"), tr("Type"), tr("Steps"),
         tr("Created By"), tr("Created At"), tr("Correlation ID")});
    instanceTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    instanceTable_->horizontalHeader()->setStretchLastSection(true);
    instanceTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    instanceTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    instanceTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    instanceTable_->setAlternatingRowColors(true);
    instanceTable_->setItemDelegate(new BadgeDelegate(instanceTable_));
    instanceTable_->verticalHeader()->setVisible(false);

    connect(instanceTable_, &QTableWidget::currentItemChanged,
            this, [this](QTableWidgetItem*, QTableWidgetItem*) {
                onInstanceSelectionChanged();
            });

    // ── Steps detail panel (bottom of splitter) ───────────────────────────────
    stepsGroup_ = new QGroupBox(tr("Steps"), tab);
    auto* stepsLayout = new QVBoxLayout(stepsGroup_);
    stepsLayout->setContentsMargins(4, 4, 4, 4);

    stepsTable_ = new QTableWidget(0, static_cast<int>(SCol::Count), stepsGroup_);
    stepsTable_->setHorizontalHeaderLabels(
        {tr("#"), tr("Name"), tr("Status"),
         tr("Started At"), tr("Completed At"), tr("Error")});
    stepsTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
    stepsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    stepsTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    stepsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    stepsTable_->setAlternatingRowColors(true);
    stepsTable_->setItemDelegate(new BadgeDelegate(stepsTable_));
    stepsTable_->verticalHeader()->setVisible(false);

    connect(stepsTable_, &QTableWidget::cellDoubleClicked,
            this, &WorkflowMdiWindow::onStepDoubleClicked);

    stepsLayout->addWidget(stepsTable_);

    // ── Splitter ──────────────────────────────────────────────────────────────
    splitter_ = new QSplitter(Qt::Vertical, tab);
    splitter_->addWidget(instanceTable_);
    splitter_->addWidget(stepsGroup_);
    splitter_->setStretchFactor(0, 2);
    splitter_->setStretchFactor(1, 1);

    layout->addWidget(splitter_);

    connect(searchEdit_, &QLineEdit::textChanged,
            this, &WorkflowMdiWindow::onFilterChanged);
    connect(statusFilter_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &WorkflowMdiWindow::onFilterChanged);
}

void WorkflowMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &WorkflowMdiWindow::onNotificationReceived);

    auto subscribe_all = [this]() {
        clientManager_->subscribeToEvent(std::string{kWorkflowChangedEvent});
    };

    connect(clientManager_, &ClientManager::loggedIn,
            this, [subscribe_all]() { subscribe_all(); });
    connect(clientManager_, &ClientManager::reconnected,
            this, [subscribe_all]() { subscribe_all(); });

    if (clientManager_->isConnected())
        subscribe_all();
}

// ─────────────────────────────────────────────────────────────────────────────
// doReload / async fetch — instances
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::doReload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    if (watcher_->isRunning()) {
        BOOST_LOG_SEV(lg(), debug) << "Fetch already in progress; skipping.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting workflow instance fetch.";

    QPointer<WorkflowMdiWindow> self = this;
    watcher_->setFuture(QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {};

        try {
            auto resp = self->clientManager_->process_authenticated_request(
                wf::list_workflow_instances_request{});
            if (!resp)
                return {false, QString::fromStdString(resp.error()), {}};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message), {}};
            return {true, {}, std::move(resp->instances)};
        } catch (const std::exception& e) {
            return {false, QString::fromLatin1(e.what()), {}};
        } catch (...) {
            return {false, QStringLiteral("Unknown error fetching workflows"), {}};
        }
    }));
}

void WorkflowMdiWindow::onFetchFinished() {
    const auto result = watcher_->result();
    endLoading();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Workflow fetch failed: " << result.error.toStdString();
        emit errorOccurred(result.error);
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Fetched " << result.instances.size() << " workflow instance(s).";

    currentInstances_ = result.instances;
    populateDashboard(currentInstances_);
    populateExecutionList(currentInstances_);
    emit statusChanged(tr("Loaded %1 workflow(s).")
        .arg(static_cast<int>(currentInstances_.size())));
}

// ─────────────────────────────────────────────────────────────────────────────
// Populate
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::populateDashboard(
    const std::vector<wf::workflow_instance_summary>& instances) {

    int active = 0, failed = 0;
    failuresTable_->setRowCount(0);

    for (const auto& inst : instances) {
        const QString status = QString::fromStdString(inst.status);
        if (status == QStringLiteral("in_progress"))
            ++active;
        else if (is_failed(status)) {
            ++failed;
            const int row = failuresTable_->rowCount();
            failuresTable_->insertRow(row);
            failuresTable_->setItem(row, static_cast<int>(FCol::Type),
                make_item(QString::fromStdString(inst.type)));
            failuresTable_->setItem(row, static_cast<int>(FCol::CreatedAt),
                make_item(QString::fromStdString(inst.created_at)));
            failuresTable_->setItem(row, static_cast<int>(FCol::Error),
                make_item(QString::fromStdString(inst.error)));
        }
    }

    activeCountLabel_->setText(QString::number(active));
    failedCountLabel_->setText(QString::number(failed));
}

void WorkflowMdiWindow::populateExecutionList(
    const std::vector<wf::workflow_instance_summary>& instances) {

    // Remember selected instance ID so we can re-select after repopulation.
    QString prevSelectedId = selectedInstanceId_;

    const QString searchText = searchEdit_->text().trimmed().toLower();
    const QString filterStatus = statusFilter_->currentData().toString();

    instanceTable_->setRowCount(0);

    int reSelectRow = -1;

    for (const auto& inst : instances) {
        const QString status = QString::fromStdString(inst.status);
        const QString type   = QString::fromStdString(inst.type);
        const QString corrId = QString::fromStdString(inst.correlation_id);
        const QString id     = QString::fromStdString(inst.id);

        // Apply status filter — map display "Failed" to all failure states.
        if (!filterStatus.isEmpty()) {
            const bool wantFailed = (filterStatus == QStringLiteral("failed"));
            if (wantFailed) {
                if (!is_failed(status)) continue;
            } else {
                if (status != filterStatus) continue;
            }
        }

        // Apply search filter (type or correlation ID).
        if (!searchText.isEmpty()) {
            if (!type.toLower().contains(searchText) &&
                !corrId.toLower().contains(searchText))
                continue;
        }

        const int row = instanceTable_->rowCount();
        instanceTable_->insertRow(row);

        // Status badge — store instance and correlation IDs on col-0.
        auto* statusItem = make_badge_item(
            display_status(status), status_color(status));
        statusItem->setData(InstanceIdRole, id);
        statusItem->setData(CorrIdRole, corrId);
        instanceTable_->setItem(row, static_cast<int>(Col::Status), statusItem);

        instanceTable_->setItem(row, static_cast<int>(Col::Type),
            make_item(type));

        instanceTable_->setItem(row, static_cast<int>(Col::Steps),
            make_item(QStringLiteral("%1/%2")
                .arg(inst.current_step_index + 1)
                .arg(inst.step_count)));

        instanceTable_->setItem(row, static_cast<int>(Col::CreatedBy),
            make_item(QString::fromStdString(inst.created_by)));

        instanceTable_->setItem(row, static_cast<int>(Col::CreatedAt),
            make_item(QString::fromStdString(inst.created_at)));

        instanceTable_->setItem(row, static_cast<int>(Col::CorrelationId),
            make_item(corrId));

        if (id == prevSelectedId)
            reSelectRow = row;
    }

    instanceTable_->resizeColumnsToContents();
    instanceTable_->horizontalHeader()->setStretchLastSection(true);

    // Re-select the previously selected row (or clear steps panel).
    if (reSelectRow >= 0) {
        instanceTable_->selectRow(reSelectRow);
    } else {
        selectedInstanceId_.clear();
        stepsTable_->setRowCount(0);
        stepsGroup_->setTitle(tr("Steps"));
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Steps — async fetch and populate
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::loadStepsForInstance(const QString& instanceId) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    if (stepsWatcher_->isRunning())
        stepsWatcher_->cancel();

    QPointer<WorkflowMdiWindow> self = this;
    const std::string id = instanceId.toStdString();

    stepsWatcher_->setFuture(QtConcurrent::run([self, id]() -> StepsFetchResult {
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
            return {false, QString::fromLatin1(e.what()), {}};
        } catch (...) {
            return {false, QStringLiteral("Unknown error loading steps"), {}};
        }
    }));
}

void WorkflowMdiWindow::onStepsFetchFinished() {
    const auto result = stepsWatcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Step fetch failed: " << result.error.toStdString();
        stepsGroup_->setTitle(
            tr("Steps — Error: %1").arg(result.error));
        return;
    }

    populateSteps(result.steps);
}

void WorkflowMdiWindow::populateSteps(
    const std::vector<wf::workflow_step_summary>& steps) {

    stepsTable_->setRowCount(0);

    for (const auto& step : steps) {
        const int row = stepsTable_->rowCount();
        stepsTable_->insertRow(row);

        stepsTable_->setItem(row, static_cast<int>(SCol::Index),
            make_item(QString::number(step.step_index + 1)));

        stepsTable_->setItem(row, static_cast<int>(SCol::Name),
            make_item(QString::fromStdString(step.name)));

        const QString status = QString::fromStdString(step.status);
        stepsTable_->setItem(row, static_cast<int>(SCol::Status),
            make_badge_item(display_status(status), status_color(status)));

        stepsTable_->setItem(row, static_cast<int>(SCol::StartedAt),
            make_item(step.started_at
                ? QString::fromStdString(*step.started_at)
                : QStringLiteral("—")));

        stepsTable_->setItem(row, static_cast<int>(SCol::CompletedAt),
            make_item(step.completed_at
                ? QString::fromStdString(*step.completed_at)
                : QStringLiteral("—")));

        // Store full error text as item data; truncate for display.
        const QString fullError = QString::fromStdString(step.error);
        auto* errorItem = make_item(fullError);
        errorItem->setData(ErrorDetailRole, fullError);
        stepsTable_->setItem(row, static_cast<int>(SCol::Error), errorItem);
    }

    stepsTable_->resizeColumnsToContents();
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
    stepsGroup_->setTitle(tr("Steps (%1)").arg(steps.size()));
}

// ─────────────────────────────────────────────────────────────────────────────
// Slots
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::onRefreshToggled(bool checked) {
    if (checked) {
        const int current_secs = autoRefreshTimer_->interval() / 1000;
        const int secs = QInputDialog::getInt(
            this, tr("Auto Refresh"),
            tr("Refresh interval (seconds):"),
            current_secs, 5, 3600, 5);
        if (secs <= 0) {
            QSignalBlocker blocker(autoRefreshAction_);
            autoRefreshAction_->setChecked(false);
            return;
        }
        autoRefreshTimer_->setInterval(secs * 1000);
        autoRefreshAction_->setToolTip(
            tr("Auto refresh every %1 s — click to disable").arg(secs));
        autoRefreshTimer_->start();
    } else {
        autoRefreshTimer_->stop();
        autoRefreshAction_->setToolTip(
            tr("Enable automatic refresh; click again to disable"));
    }
}

void WorkflowMdiWindow::onInstanceSelectionChanged() {
    const int row = instanceTable_->currentRow();
    if (row < 0) {
        selectedInstanceId_.clear();
        stepsTable_->setRowCount(0);
        stepsGroup_->setTitle(tr("Steps"));
        return;
    }

    auto* statusItem = instanceTable_->item(row, static_cast<int>(Col::Status));
    if (!statusItem) return;

    const QString id = statusItem->data(InstanceIdRole).toString();
    if (id.isEmpty() || id == selectedInstanceId_)
        return;

    selectedInstanceId_ = id;

    const QString corrId = statusItem->data(CorrIdRole).toString();
    auto* typeItem = instanceTable_->item(row, static_cast<int>(Col::Type));
    const QString type = typeItem ? typeItem->text() : QString{};

    stepsGroup_->setTitle(tr("Steps — %1  [%2]").arg(type, corrId));
    stepsTable_->setRowCount(0);
    loadStepsForInstance(id);
}

void WorkflowMdiWindow::onStepDoubleClicked(int row, int /*col*/) {
    auto* errorItem = stepsTable_->item(row, static_cast<int>(SCol::Error));
    if (!errorItem) return;

    const QString fullError = errorItem->data(ErrorDetailRole).toString();

    // Always show the dialog (even without an error) so the user can see step info.
    auto* nameItem = stepsTable_->item(row, static_cast<int>(SCol::Name));
    const QString stepName = nameItem ? nameItem->text() : QString{};

    // Find correlation ID and workflow ID from the selected instance row.
    const int instRow = instanceTable_->currentRow();
    QString workflowId, corrId;
    if (instRow >= 0) {
        auto* si = instanceTable_->item(instRow, static_cast<int>(Col::Status));
        if (si) {
            workflowId = si->data(InstanceIdRole).toString();
            corrId     = si->data(CorrIdRole).toString();
        }
    }

    auto* dlg = new QDialog(this);
    dlg->setAttribute(Qt::WA_DeleteOnClose);
    dlg->setWindowTitle(tr("Step details — %1").arg(stepName));
    dlg->resize(760, 480);

    auto* layout = new QVBoxLayout(dlg);

    // ── Header: workflow ID and correlation ID (copyable) ─────────────────────
    auto* headerLayout = new QHBoxLayout;
    auto makeCopyLabel = [dlg](const QString& label, const QString& value) {
        auto* l = new QLabel(
            QStringLiteral("<b>%1:</b> %2").arg(label, value), dlg);
        l->setTextInteractionFlags(Qt::TextSelectableByMouse);
        return l;
    };
    headerLayout->addWidget(makeCopyLabel(tr("Workflow ID"), workflowId));
    headerLayout->addWidget(makeCopyLabel(tr("Correlation ID"), corrId));
    headerLayout->addStretch();
    layout->addLayout(headerLayout);

    // ── Error text ────────────────────────────────────────────────────────────
    const QFont fixed = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    auto* edit = new QPlainTextEdit(
        fullError.isEmpty() ? tr("(no error)") : fullError, dlg);
    edit->setReadOnly(true);
    edit->setFont(fixed);
    layout->addWidget(edit);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, dlg);
    connect(buttons, &QDialogButtonBox::rejected, dlg, &QDialog::accept);
    layout->addWidget(buttons);

    dlg->show();
}

void WorkflowMdiWindow::onNotificationReceived(
    const QString& eventType,
    const QDateTime& /*timestamp*/,
    const QStringList& /*entityIds*/,
    const QString& /*tenantId*/) {

    if (eventType == QLatin1String(kWorkflowChangedEvent))
        markAsStale();
}

void WorkflowMdiWindow::onFilterChanged() {
    populateExecutionList(currentInstances_);
}

} // namespace ores::qt
