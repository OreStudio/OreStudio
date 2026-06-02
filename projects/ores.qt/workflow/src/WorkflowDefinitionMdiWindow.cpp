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
#include "ores.qt/WorkflowDefinitionMdiWindow.hpp"

#include <QAction>
#include <QCloseEvent>
#include <QGroupBox>
#include <QHeaderView>
#include <QLabel>
#include <QPointer>
#include <QSplitter>
#include <QToolBar>
#include <QVBoxLayout>
#include <QWidget>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace wf = ores::workflow::messaging;

namespace {

constexpr auto kSettingsGroup = "WorkflowDefinitionMdiWindow";

// Definition table columns
enum class DCol {
    TypeName = 0,
    Description,
    StepCount,
    Count
};

// Steps detail table columns
enum class SCol {
    Index = 0,
    Name,
    Description,
    CommandSubject,
    Compensation,
    Count
};

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

} // namespace

// ─────────────────────────────────────────────────────────────────────────────
// WorkflowDefinitionMdiWindow
// ─────────────────────────────────────────────────────────────────────────────

WorkflowDefinitionMdiWindow::WorkflowDefinitionMdiWindow(
    ClientManager* clientManager, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      definitionTable_(nullptr),
      splitter_(nullptr),
      stepsGroup_(nullptr),
      stepsTable_(nullptr),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &WorkflowDefinitionMdiWindow::onFetchFinished);

    setupUi();

    UiPersistence::restoreSize(
        QLatin1String(kSettingsGroup), {800, 500});
    UiPersistence::restoreSplitter(
        QLatin1String(kSettingsGroup), splitter_);

    reload();
}

void WorkflowDefinitionMdiWindow::closeEvent(QCloseEvent* event) {
    UiPersistence::saveSize(QLatin1String(kSettingsGroup), this);
    UiPersistence::saveSplitter(QLatin1String(kSettingsGroup), splitter_);
    EntityListMdiWindow::closeEvent(event);
}

void WorkflowDefinitionMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    // ── Definition table (top of splitter) ──────────────────────────────────
    definitionTable_ = new QTableWidget(
        0, static_cast<int>(DCol::Count), this);
    definitionTable_->setHorizontalHeaderLabels(
        {tr("Type Name"), tr("Description"), tr("Steps")});
    definitionTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    definitionTable_->horizontalHeader()->setStretchLastSection(true);
    definitionTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    definitionTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    definitionTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    definitionTable_->setAlternatingRowColors(true);
    definitionTable_->verticalHeader()->setVisible(false);

    connect(definitionTable_, &QTableWidget::currentItemChanged,
            this, [this](QTableWidgetItem*, QTableWidgetItem*) {
                onDefinitionSelectionChanged();
            });

    // ── Steps detail panel (bottom of splitter) ─────────────────────────────
    stepsGroup_ = new QGroupBox(tr("Steps"), this);
    auto* stepsLayout = new QVBoxLayout(stepsGroup_);
    stepsLayout->setContentsMargins(4, 4, 4, 4);

    stepsTable_ = new QTableWidget(
        0, static_cast<int>(SCol::Count), stepsGroup_);
    stepsTable_->setHorizontalHeaderLabels(
        {tr("#"), tr("Name"), tr("Description"),
         tr("Command Subject"), tr("Compensation")});
    stepsTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
    stepsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    stepsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    stepsTable_->setAlternatingRowColors(true);
    stepsTable_->verticalHeader()->setVisible(false);

    stepsLayout->addWidget(stepsTable_);

    // ── Splitter ─────────────────────────────────────────────────────────────
    splitter_ = new QSplitter(Qt::Vertical, this);
    splitter_->addWidget(definitionTable_);
    splitter_->addWidget(stepsGroup_);
    splitter_->setStretchFactor(0, 1);
    splitter_->setStretchFactor(1, 2);

    layout->addWidget(splitter_);
}

void WorkflowDefinitionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setIconSize(QSize(16, 16));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh workflow definitions"));
    connect(refreshAction_, &QAction::triggered,
            this, &WorkflowDefinitionMdiWindow::reload);

    initializeStaleIndicator(refreshAction_,
        IconUtils::iconPath(Icon::ArrowSync));
}

// ─────────────────────────────────────────────────────────────────────────────
// doReload / async fetch
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowDefinitionMdiWindow::doReload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    if (watcher_->isRunning()) {
        BOOST_LOG_SEV(lg(), debug) << "Fetch already in progress; skipping.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting workflow definitions fetch.";

    QPointer<WorkflowDefinitionMdiWindow> self = this;
    watcher_->setFuture(QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {};

        try {
            auto resp = self->clientManager_->process_authenticated_request(
                wf::list_workflow_definitions_request{});
            if (!resp)
                return {false, QString::fromStdString(resp.error()), {}};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message), {}};
            return {true, {}, std::move(resp->definitions)};
        } catch (const std::exception& e) {
            return {false, QString::fromLatin1(e.what()), {}};
        } catch (...) {
            return {false,
                QStringLiteral("Unknown error fetching definitions"), {}};
        }
    }));
}

void WorkflowDefinitionMdiWindow::onFetchFinished() {
    const auto result = watcher_->result();
    endLoading();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Definitions fetch failed: " << result.error.toStdString();
        emit errorOccurred(result.error);
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Fetched " << result.definitions.size()
        << " workflow definition(s).";

    currentDefinitions_ = result.definitions;
    populateDefinitions(currentDefinitions_);
    emit statusChanged(tr("Loaded %1 workflow definition(s).")
        .arg(static_cast<int>(currentDefinitions_.size())));
}

// ─────────────────────────────────────────────────────────────────────────────
// Populate
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowDefinitionMdiWindow::populateDefinitions(
    const std::vector<wf::workflow_definition_summary>& definitions) {

    definitionTable_->setRowCount(0);

    for (const auto& def : definitions) {
        const int row = definitionTable_->rowCount();
        definitionTable_->insertRow(row);

        definitionTable_->setItem(row, static_cast<int>(DCol::TypeName),
            make_item(QString::fromStdString(def.type_name)));
        definitionTable_->setItem(row, static_cast<int>(DCol::Description),
            make_item(QString::fromStdString(def.description)));
        definitionTable_->setItem(row, static_cast<int>(DCol::StepCount),
            make_item(QString::number(def.step_count)));
    }

    definitionTable_->resizeColumnsToContents();
    definitionTable_->horizontalHeader()->setStretchLastSection(true);

    // Select the first row if any definitions exist.
    if (definitionTable_->rowCount() > 0)
        definitionTable_->selectRow(0);
    else {
        stepsTable_->setRowCount(0);
        stepsGroup_->setTitle(tr("Steps"));
    }
}

void WorkflowDefinitionMdiWindow::populateSteps(
    const std::vector<wf::workflow_step_definition_summary>& steps) {

    stepsTable_->setRowCount(0);

    for (const auto& step : steps) {
        const int row = stepsTable_->rowCount();
        stepsTable_->insertRow(row);

        stepsTable_->setItem(row, static_cast<int>(SCol::Index),
            make_item(QString::number(step.step_index + 1)));
        stepsTable_->setItem(row, static_cast<int>(SCol::Name),
            make_item(QString::fromStdString(step.name)));
        stepsTable_->setItem(row, static_cast<int>(SCol::Description),
            make_item(QString::fromStdString(step.description)));
        stepsTable_->setItem(row, static_cast<int>(SCol::CommandSubject),
            make_item(QString::fromStdString(step.command_subject)));
        stepsTable_->setItem(row, static_cast<int>(SCol::Compensation),
            make_item(step.has_compensation
                ? tr("Yes") : tr("No")));
    }

    stepsTable_->resizeColumnsToContents();
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
}

// ─────────────────────────────────────────────────────────────────────────────
// Slots
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowDefinitionMdiWindow::onDefinitionSelectionChanged() {
    const int row = definitionTable_->currentRow();
    if (row < 0 || row >= static_cast<int>(currentDefinitions_.size())) {
        stepsTable_->setRowCount(0);
        stepsGroup_->setTitle(tr("Steps"));
        return;
    }

    const auto& def = currentDefinitions_[static_cast<std::size_t>(row)];
    stepsGroup_->setTitle(tr("Steps — %1")
        .arg(QString::fromStdString(def.type_name)));
    populateSteps(def.steps);
}

} // namespace ores::qt
