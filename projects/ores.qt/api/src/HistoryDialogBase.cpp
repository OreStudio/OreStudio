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
#include "ores.diff/domain/field_value.hpp"
#include "ores.diff/engine/compare.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include <QAction>
#include <QHeaderView>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QTableWidget>
#include <QTimer>
#include <QToolBar>
#include <QVBoxLayout>
#include <algorithm>

namespace ores::qt {

namespace {

const QIcon& historyIcon() {
    static const QIcon icon =
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor);
    return icon;
}

const QIcon& reloadIcon() {
    static const QIcon icon =
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor);
    return icon;
}

const QIcon& reloadStaleIcon() {
    static const QIcon icon =
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, color_constants::stale_indicator);
    return icon;
}

// GitHub dark-theme diff palette: line background is a faint tint,
// intra-line/token highlight is a stronger shade of the same colour.
constexpr auto old_line_bg = "rgba(248,81,73,.15)";
constexpr auto old_span_bg = "rgba(248,81,73,.40)";
constexpr auto new_line_bg = "rgba(63,185,80,.15)";
constexpr auto new_span_bg = "rgba(46,160,67,.40)";

QString escapeAndWrapNewlines(const QString& text) {
    QString escaped = text.toHtmlEscaped();
    escaped.replace(QLatin1String("\n"), QLatin1String("<br/>"));
    return escaped;
}

// Renders one side of a diff_entry as HTML: plain text with the
// changed byte ranges (spans, in the original UTF-8 std::string's
// byte offsets — sliced before conversion to QString so a multi-byte
// code point is never split) wrapped in a highlighted <span>.
QString renderSpannedHtml(const std::string& text,
                          const std::vector<ores::diff::domain::diff_span>& spans,
                          const char* span_bg) {
    if (spans.empty())
        return escapeAndWrapNewlines(QString::fromStdString(text));

    auto sorted_spans = spans;
    std::sort(sorted_spans.begin(), sorted_spans.end(),
              [](const auto& a, const auto& b) { return a.offset < b.offset; });

    QString html;
    std::size_t pos = 0;
    for (const auto& span : sorted_spans) {
        if (span.offset > pos)
            html += escapeAndWrapNewlines(
                QString::fromStdString(text.substr(pos, span.offset - pos)));
        const auto highlighted = text.substr(span.offset, span.length);
        html += QString("<span style=\"background-color:%1;\">%2</span>")
                    .arg(span_bg, escapeAndWrapNewlines(QString::fromStdString(highlighted)));
        pos = span.offset + span.length;
    }
    if (pos < text.size())
        html += escapeAndWrapNewlines(QString::fromStdString(text.substr(pos)));
    return html;
}

// Wraps one side's spanned HTML in the line-level background.
QString renderDiffCell(const std::string& text,
                       const std::vector<ores::diff::domain::diff_span>& spans,
                       const char* line_bg,
                       const char* span_bg) {
    const QString inner =
        text.empty() ? QString() : renderSpannedHtml(text, spans, span_bg);
    return QString("<div style=\"background-color:%1; white-space:pre-wrap; padding:2px;\">%2</div>")
        .arg(line_bg, inner);
}

QLabel* makeDiffLabel(QWidget* parent, const QString& html) {
    auto* label = new QLabel(parent);
    label->setTextFormat(Qt::RichText);
    label->setWordWrap(true);
    label->setText(html);
    return label;
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
        QString("%1 has new versions - click Reload to see them").arg(code()));
    if (reloadPulse_)
        reloadPulse_->start_pulsing();
}

void HistoryDialogBase::onReloadPulseStateChanged(bool is_on) {
    if (reloadAction_)
        reloadAction_->setIcon(is_on ? reloadStaleIcon() : reloadIcon());
}

QSize HistoryDialogBase::sizeHint() const {
    const QSize base = QWidget::sizeHint();
    return {qMax(base.width(), 900), qMax(base.height(), 600)};
}

void HistoryDialogBase::initializeHistoryUi(const HistoryWidgets& widgets) {
    widgets_ = widgets;

    setupToolbar();

    reloadPulse_ = new RecencyPulseManager(this);
    connect(reloadPulse_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &HistoryDialogBase::onReloadPulseStateChanged);

    if (widgets_.versionList) {
        connect(widgets_.versionList,
                &QTableWidget::currentCellChanged,
                this,
                [this](int currentRow, int, int, int) { onVersionSelectedRow(currentRow); });
        connect(widgets_.versionList, &QTableWidget::cellDoubleClicked, this, [this](int, int) {
            onOpenClicked();
        });

        widgets_.versionList->setAlternatingRowColors(true);
        widgets_.versionList->setSelectionMode(QAbstractItemView::SingleSelection);
        widgets_.versionList->setSelectionBehavior(QAbstractItemView::SelectRows);
        widgets_.versionList->resizeRowsToContents();
        widgets_.versionList->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
        widgets_.versionList->horizontalHeader()->setSectionResizeMode(
            QHeaderView::ResizeToContents);
    }

    if (widgets_.changesTable) {
        widgets_.changesTable->horizontalHeader()->setStretchLastSection(true);
        widgets_.changesTable->setColumnWidth(0, 200);
        widgets_.changesTable->setColumnWidth(1, 200);
    }

    if (widgets_.closeButton) {
        widgets_.closeButton->setIcon(
            IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
        connect(
            widgets_.closeButton, &QPushButton::clicked, this, &HistoryDialogBase::requestClose);
    }

    updateActionStates();
}

void HistoryDialogBase::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    reloadAction_ = new QAction(tr("Reload"), this);
    reloadAction_->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip(tr("Reload history from server"));
    connect(reloadAction_, &QAction::triggered, this, &HistoryDialogBase::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    openAction_ = new QAction(tr("Open"), this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip(tr("Open this version in read-only mode"));
    connect(openAction_, &QAction::triggered, this, &HistoryDialogBase::onOpenClicked);
    toolBar_->addAction(openAction_);

    revertAction_ = new QAction(tr("Revert"), this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                          IconUtils::DefaultIconColor));
    revertAction_->setToolTip(tr("Revert to this version"));
    connect(revertAction_, &QAction::triggered, this, &HistoryDialogBase::onRevertClicked);
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
            widgets_.versionList->setItem(i, j + 1, new QTableWidgetItem(row.cells[j]));
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
    emit errorOccurred(tr("Failed to load history: %1").arg(error_message));
    MessageBoxHelper::critical(
        this, tr("History Load Error"), tr("Failed to load history:\n%1").arg(error_message));
}

int HistoryDialogBase::selectedVersionIndex() const {
    return widgets_.versionList ? widgets_.versionList->currentRow() : -1;
}

HistoryDialogBase::VersionRow HistoryDialogBase::versionRow(int) const {
    return {};
}

HistoryDialogBase::DiffResult HistoryDialogBase::calculateDiffAt(int, int) const {
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

    auto placeholderRow = [this](const QString& text) {
        widgets_.changesTable->setRowCount(1);
        widgets_.changesTable->setItem(0, 0, new QTableWidgetItem(text));
        widgets_.changesTable->setItem(0, 1, new QTableWidgetItem("-"));
        widgets_.changesTable->setItem(0, 2, new QTableWidgetItem("-"));
    };

    // The oldest version has nothing to diff against.
    if (version_index == historySize() - 1) {
        placeholderRow(tr("(Initial version)"));
        return;
    }

    const DiffResult diffs = calculateDiffAt(version_index, version_index + 1);

    if (diffs.isEmpty()) {
        placeholderRow(tr("(No field changes)"));
        return;
    }

    widgets_.changesTable->setRowCount(diffs.size());
    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        widgets_.changesTable->setItem(i, 0, new QTableWidgetItem(field));

        // Compute the intra-value spans centrally (the same
        // entity-agnostic ores.diff::engine algorithm the server uses)
        // so every dialog gets GitHub-style highlighting from a flat
        // DiffResult, regardless of whether its calculateDiffAt() came
        // from the generic HistoryDialog or a legacy per-entity one.
        using ores::diff::domain::field_value;
        const auto field_std = field.toStdString();
        const auto changes = ores::diff::engine::compute({field_value{field_std, old_val.toStdString()}},
                                                          {field_value{field_std, new_val.toStdString()}});
        const auto* entry = changes.entries.empty() ? nullptr : &changes.entries.front();

        if (QWidget* old_widget = changeCellWidget(field, old_val)) {
            widgets_.changesTable->setCellWidget(i, 1, old_widget);
        } else if (entry) {
            widgets_.changesTable->setCellWidget(
                i, 1,
                makeDiffLabel(widgets_.changesTable,
                             renderDiffCell(entry->old_value, entry->old_spans, old_line_bg, old_span_bg)));
        } else {
            widgets_.changesTable->setItem(i, 1, new QTableWidgetItem(old_val));
        }

        if (QWidget* new_widget = changeCellWidget(field, new_val)) {
            widgets_.changesTable->setCellWidget(i, 2, new_widget);
        } else if (entry) {
            widgets_.changesTable->setCellWidget(
                i, 2,
                makeDiffLabel(widgets_.changesTable,
                             renderDiffCell(entry->new_value, entry->new_spans, new_line_bg, new_span_bg)));
        } else {
            widgets_.changesTable->setItem(i, 2, new QTableWidgetItem(new_val));
        }
    }
    widgets_.changesTable->resizeRowsToContents();

    // A freshly-inserted QLabel cell widget's sizeHint() isn't yet
    // based on its actual (wrapped, laid-out) content in this same
    // event-loop turn, so the resizeRowsToContents() above under-sizes
    // multiline rows. Defer a second pass to let Qt lay the widgets
    // out first, then resize again against their now-correct sizeHint.
    QPointer<QTableWidget> table = widgets_.changesTable;
    QTimer::singleShot(0, table, [table]() {
        if (table)
            table->resizeRowsToContents();
    });
}

void HistoryDialogBase::updateActionStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 && index < historySize();

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    // Reverting to the latest version is a no-op, so the action only
    // makes sense for older versions.
    if (revertAction_)
        revertAction_->setEnabled(hasSelection && index > 0);
}

void HistoryDialogBase::onReloadClicked() {
    if (reloadPulse_)
        reloadPulse_->stop_pulsing();
    onReloadPulseStateChanged(false);
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
    // Reverting restores the SELECTED version; the action is disabled
    // for the latest version, where reverting would be a no-op.
    const int index = selectedVersionIndex();
    if (index <= 0 || index >= historySize())
        return;

    const VersionRow latest = versionRow(0);
    const VersionRow selected = versionRow(index);

    const auto reply =
        MessageBoxHelper::question(this,
                                   tr("Revert"),
                                   tr("Are you sure you want to revert '%1' from version %2 back "
                                      "to version %3?\n\nThis will create a new version with the "
                                      "data from version %3.")
                                       .arg(code())
                                       .arg(latest.version)
                                       .arg(selected.version),
                                   QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes)
        return;

    revertToVersionAt(index);
}

void HistoryDialogBase::checkString(DiffResult& diffs,
                                    const QString& field,
                                    const std::string& current,
                                    const std::string& previous) {
    if (current != previous) {
        diffs.append({field, {QString::fromStdString(previous), QString::fromStdString(current)}});
    }
}

void HistoryDialogBase::checkString(DiffResult& diffs,
                                    const QString& field,
                                    const std::optional<std::string>& current,
                                    const std::optional<std::string>& previous) {
    if (current != previous) {
        auto text = [](const std::optional<std::string>& v) {
            return v ? QString::fromStdString(*v) : QString();
        };
        diffs.append({field, {text(previous), text(current)}});
    }
}

void HistoryDialogBase::checkDouble(DiffResult& diffs,
                                    const QString& field,
                                    double current,
                                    double previous) {
    if (current != previous) {
        diffs.append({field, {QString::number(previous), QString::number(current)}});
    }
}

void HistoryDialogBase::checkInt(DiffResult& diffs,
                                 const QString& field,
                                 int current,
                                 int previous) {
    if (current != previous) {
        diffs.append({field, {QString::number(previous), QString::number(current)}});
    }
}

void HistoryDialogBase::checkInt(DiffResult& diffs,
                                 const QString& field,
                                 const std::optional<int>& current,
                                 const std::optional<int>& previous) {
    if (current != previous) {
        auto text = [](const std::optional<int>& v) {
            return v ? QString::number(*v) : QString();
        };
        diffs.append({field, {text(previous), text(current)}});
    }
}

void HistoryDialogBase::checkBool(DiffResult& diffs,
                                  const QString& field,
                                  bool current,
                                  bool previous) {
    if (current != previous) {
        auto text = [](bool b) {
            return b ? tr("Yes") : tr("No");
        };
        diffs.append({field, {text(previous), text(current)}});
    }
}

}
