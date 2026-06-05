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
#include "ores.qt/HistoryDialogBase.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

#include <QAction>
#include <QHeaderView>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QTableWidget>
#include <QToolBar>
#include <QVBoxLayout>

namespace ores::qt {

namespace {

const QIcon& historyIcon() {
    static const QIcon icon = IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor);
    return icon;
}

}

HistoryDialogBase::~HistoryDialogBase() {
    // Disconnect and cancel any active QFutureWatcher objects so late
    // results cannot land on a destroyed widget.
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void HistoryDialogBase::markAsStale() {
    emit statusChanged(
        QString("%1 was modified - reloading history...").arg(code()));
    loadHistory();
}

QSize HistoryDialogBase::sizeHint() const {
    const QSize base = QWidget::sizeHint();
    return {qMax(base.width(), 900), qMax(base.height(), 600)};
}

void HistoryDialogBase::initializeHistoryUi(const HistoryWidgets& widgets) {
    widgets_ = widgets;

    setupToolbar();

    if (widgets_.versionList) {
        connect(widgets_.versionList, &QTableWidget::currentCellChanged, this,
                [this](int currentRow, int, int, int) {
                    onVersionSelectedRow(currentRow);
                });
        connect(widgets_.versionList, &QTableWidget::cellDoubleClicked, this,
                [this](int, int) { onOpenClicked(); });

        widgets_.versionList->setAlternatingRowColors(true);
        widgets_.versionList->setSelectionMode(
            QAbstractItemView::SingleSelection);
        widgets_.versionList->setSelectionBehavior(
            QAbstractItemView::SelectRows);
        widgets_.versionList->resizeRowsToContents();
        widgets_.versionList->verticalHeader()->setSectionResizeMode(
            QHeaderView::ResizeToContents);
        widgets_.versionList->horizontalHeader()->setSectionResizeMode(
            QHeaderView::ResizeToContents);
    }

    if (widgets_.changesTable) {
        widgets_.changesTable->horizontalHeader()->setStretchLastSection(true);
        widgets_.changesTable->setColumnWidth(0, 200);
        widgets_.changesTable->setColumnWidth(1, 200);
    }

    if (widgets_.closeButton) {
        widgets_.closeButton->setIcon(IconUtils::createRecoloredIcon(
            Icon::Dismiss, IconUtils::DefaultIconColor));
        connect(widgets_.closeButton, &QPushButton::clicked, this, [this]() {
            if (window())
                window()->close();
        });
    }

    updateActionStates();
}

void HistoryDialogBase::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
            &HistoryDialogBase::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
            &HistoryDialogBase::onOpenClicked);
    toolBar_->addAction(openAction_);

    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert to this version");
    connect(revertAction_, &QAction::triggered, this,
            &HistoryDialogBase::onRevertClicked);
    toolBar_->addAction(revertAction_);

    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void HistoryDialogBase::historyLoaded() {
    if (!widgets_.versionList)
        return;

    const int size = historySize();
    const QIcon& icon = historyIcon();

    widgets_.versionList->setRowCount(0);
    widgets_.versionList->setRowCount(size);

    for (int i = 0; i < size; ++i) {
        const VersionRow row = versionRow(i);

        auto* versionItem = new QTableWidgetItem(QString::number(row.version));
        versionItem->setIcon(icon);
        widgets_.versionList->setItem(i, 0, versionItem);

        for (int j = 0; j < row.cells.size(); ++j) {
            widgets_.versionList->setItem(i, j + 1,
                new QTableWidgetItem(row.cells[j]));
        }
    }

    if (size > 0) {
        widgets_.versionList->selectRow(0);
        if (widgets_.titleLabel)
            widgets_.titleLabel->setText(historyTitle());
    }

    updateActionStates();

    emit statusChanged(QString("Loaded %1 versions").arg(size));
}

void HistoryDialogBase::historyLoadFailed(const QString& error_message) {
    emit errorOccurred(
        QString("Failed to load history: %1").arg(error_message));
    MessageBoxHelper::critical(
        this, "History Load Error",
        QString("Failed to load history:\n%1").arg(error_message));
}

int HistoryDialogBase::selectedVersionIndex() const {
    return widgets_.versionList ? widgets_.versionList->currentRow() : -1;
}

HistoryDialogBase::VersionRow HistoryDialogBase::versionRow(int) const {
    return {};
}

HistoryDialogBase::DiffResult
HistoryDialogBase::calculateDiffAt(int, int) const {
    return {};
}

void HistoryDialogBase::displayFullDetails(int) {}

void HistoryDialogBase::openVersionAt(int) {}

void HistoryDialogBase::revertToVersionAt(int) {}

QWidget* HistoryDialogBase::changeCellWidget(const QString&, const QString&) {
    return nullptr;
}

void HistoryDialogBase::onVersionSelectedRow(int row) {
    if (row < 0 || row >= historySize())
        return;

    displayChangesTab(row);
    displayFullDetails(row);
    updateActionStates();
}

void HistoryDialogBase::displayChangesTab(int version_index) {
    if (!widgets_.changesTable)
        return;

    widgets_.changesTable->setRowCount(0);

    if (version_index >= historySize())
        return;

    // The oldest version has nothing to diff against.
    if (version_index == historySize() - 1)
        return;

    const DiffResult diffs =
        calculateDiffAt(version_index, version_index + 1);

    widgets_.changesTable->setRowCount(diffs.size());
    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        widgets_.changesTable->setItem(i, 0, new QTableWidgetItem(field));

        if (QWidget* old_widget = changeCellWidget(field, old_val))
            widgets_.changesTable->setCellWidget(i, 1, old_widget);
        else
            widgets_.changesTable->setItem(i, 1,
                new QTableWidgetItem(old_val));

        if (QWidget* new_widget = changeCellWidget(field, new_val))
            widgets_.changesTable->setCellWidget(i, 2, new_widget);
        else
            widgets_.changesTable->setItem(i, 2,
                new QTableWidgetItem(new_val));
    }
}

void HistoryDialogBase::updateActionStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 && index < historySize();

    if (openAction_)
        openAction_->setEnabled(hasSelection);
    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

void HistoryDialogBase::onReloadClicked() {
    emit statusChanged(QString("Reloading history for %1...").arg(code()));
    loadHistory();
}

void HistoryDialogBase::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= historySize())
        return;

    openVersionAt(index);
}

void HistoryDialogBase::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= historySize())
        return;

    if (index == historySize() - 1) {
        MessageBoxHelper::information(
            this, "Cannot Revert",
            "This is the oldest version. There is no previous version to "
            "revert to.");
        return;
    }

    const VersionRow current = versionRow(index);
    const VersionRow previous = versionRow(index + 1);

    const auto reply = MessageBoxHelper::question(
        this, "Revert",
        QString("Are you sure you want to revert '%1' from version %2 back "
                "to version %3?\n\nThis will create a new version with the "
                "data from version %3.")
            .arg(code())
            .arg(current.version)
            .arg(previous.version),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes)
        return;

    revertToVersionAt(index);
}

void HistoryDialogBase::checkString(DiffResult& diffs, const QString& field,
                                    const std::string& current,
                                    const std::string& previous) {
    if (current != previous) {
        diffs.append({field,
                      {QString::fromStdString(previous),
                       QString::fromStdString(current)}});
    }
}

void HistoryDialogBase::checkInt(DiffResult& diffs, const QString& field,
                                 int current, int previous) {
    if (current != previous) {
        diffs.append({field,
                      {QString::number(previous), QString::number(current)}});
    }
}

void HistoryDialogBase::checkBool(DiffResult& diffs, const QString& field,
                                  bool current, bool previous) {
    if (current != previous) {
        auto text = [](bool b) { return b ? QString("Yes") : QString("No"); };
        diffs.append({field, {text(previous), text(current)}});
    }
}

}
