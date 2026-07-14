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
#include <QAbstractItemView>
#include <QButtonGroup>
#include <QComboBox>
#include <QHeaderView>
#include <QItemSelectionModel>
#include <QLabel>
#include <QListWidget>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTableWidget>
#include <QTextBrowser>
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

QString cssRgba(const QColor& color) {
    return QString("rgba(%1,%2,%3,%4)")
        .arg(color.red())
        .arg(color.green())
        .arg(color.blue())
        .arg(color.alphaF());
}

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
                          const QColor& span_bg) {
    if (spans.empty())
        return escapeAndWrapNewlines(QString::fromStdString(text));

    auto sorted_spans = spans;
    std::sort(sorted_spans.begin(), sorted_spans.end(), [](const auto& a, const auto& b) {
        return a.offset < b.offset;
    });

    const QString span_bg_css = cssRgba(span_bg);
    QString html;
    std::size_t pos = 0;
    for (const auto& span : sorted_spans) {
        if (span.offset > pos)
            html +=
                escapeAndWrapNewlines(QString::fromStdString(text.substr(pos, span.offset - pos)));
        const auto highlighted = text.substr(span.offset, span.length);
        html += QString("<span style=\"background-color:%1;\">%2</span>")
                    .arg(span_bg_css, escapeAndWrapNewlines(QString::fromStdString(highlighted)));
        pos = span.offset + span.length;
    }
    if (pos < text.size())
        html += escapeAndWrapNewlines(QString::fromStdString(text.substr(pos)));
    return html;
}

// Wraps one side's spanned HTML in the line-level background.
QString renderDiffCell(const std::string& text,
                       const std::vector<ores::diff::domain::diff_span>& spans,
                       const QColor& line_bg,
                       const QColor& span_bg) {
    const QString inner = text.empty() ? QString() : renderSpannedHtml(text, spans, span_bg);
    return QString(
               "<div style=\"background-color:%1; white-space:pre-wrap; padding:2px;\">%2</div>")
        .arg(cssRgba(line_bg), inner);
}

QLabel* makeDiffLabel(QWidget* parent, const QString& html) {
    auto* label = new QLabel(parent);
    label->setTextFormat(Qt::RichText);
    label->setWordWrap(true);
    label->setText(html);
    return label;
}

// A "Label: value" line where the label is muted (palette(placeholderText))
// and the value takes on its own distinct colour — so the label reads as
// a caption, not as part of the value, and the value is never confused
// with plain body text.
QLabel* makeTimelineFieldLabel(QWidget* parent,
                               const QString& label,
                               const QString& value,
                               const QColor& value_color) {
    auto* widget = new QLabel(parent);
    widget->setWordWrap(true);
    widget->setTextFormat(Qt::RichText);
    QFont font = widget->font();
    font.setPointSize(qMax(font.pointSize() - 1, 7));
    widget->setFont(font);
    widget->setText(QString("<span style='color:%1;'>%2:</span> "
                            "<span style='color:%3;'>%4</span>")
                        .arg(cssRgba(parent->palette().color(QPalette::PlaceholderText)), label,
                             cssRgba(value_color), value.toHtmlEscaped()));
    return widget;
}

// A small pill: the reason code alone, coloured but unlabelled — the
// amber fill already reads as "this is a tag", so a literal "REASON"
// prefix is redundant. Same visual language as the badge chips used
// elsewhere for code-domain values.
QLabel* makeTimelineReasonBadge(QWidget* parent, const QString& reason) {
    auto* label = new QLabel(QString(" %1 ").arg(reason), parent);
    QFont font = label->font();
    font.setPointSize(qMax(font.pointSize() - 1, 7));
    font.setBold(true);
    label->setFont(font);
    label->setStyleSheet(QString("background: %1; color: %2; border-radius: 3px; padding: 1px 4px;")
                             .arg(cssRgba(color_constants::timeline_reason_bg),
                                  cssRgba(color_constants::timeline_reason_text)));
    label->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    // TODO: show the change reason's description on hover once a
    // client-side reason-code -> description lookup is available to
    // this widget (HistoryDialogBase has no ClientManager/cache
    // access today); for now the code itself is the best we can show.
    label->setToolTip(reason);
    return label;
}

// One timeline entry: "vN · <time>" bold (hover shows the full
// Recorded At as an ISO timestamp); then, when supplied by the
// dialog's versionRow(), the provenance lines that describe who/why a
// version was recorded rather than what changed in it — matching the
// Gemini mockup's card-per-version look rather than a grid, and git
// log's "commit metadata beside the timeline, not mixed into the
// diff" convention (displayDiffPane leaves these fields out of the
// diff table entirely). cells[0] is the relative time (title line);
// cells[1]/cells[2]/cells[3]/cells[4] are the raw Modified By /
// Performed By / Change Reason Code / Change Commentary values
// (blank when the dialog doesn't supply them), each styled distinctly
// so the card doesn't read as one undifferentiated block of grey
// text; cells[5] is the full ISO timestamp for the title's tooltip.
// The selected row's accent bar/background comes from QListWidget's
// own selection styling.
QWidget* makeTimelineItemWidget(QWidget* parent, int version, const QStringList& cells) {
    auto* container = new QWidget(parent);
    auto* item_layout = new QVBoxLayout(container);
    item_layout->setContentsMargins(10, 8, 10, 8);
    item_layout->setSpacing(3);

    auto* title = new QLabel(QString("v%1 · %2").arg(version).arg(cells.value(0)), container);
    QFont title_font = title->font();
    title_font.setBold(true);
    title->setFont(title_font);
    if (const QString iso_timestamp = cells.value(5); !iso_timestamp.isEmpty())
        title->setToolTip(iso_timestamp);
    item_layout->addWidget(title);

    const QColor value_color = container->palette().color(QPalette::Text);
    if (const QString author = cells.value(1); !author.isEmpty())
        item_layout->addWidget(
            makeTimelineFieldLabel(container, QStringLiteral("Modified By"), author, value_color));

    if (const QString performed_by = cells.value(2); !performed_by.isEmpty())
        item_layout->addWidget(makeTimelineFieldLabel(container, QStringLiteral("Performed By"), performed_by,
                                                       color_constants::timeline_performed_by));

    if (const QString reason = cells.value(3); !reason.isEmpty())
        item_layout->addWidget(makeTimelineReasonBadge(container, reason), 0, Qt::AlignLeft);

    if (const QString commentary = cells.value(4); !commentary.isEmpty()) {
        auto* comment_label = new QLabel(
            QString("<span style='color:%1;'>“%2”</span>")
                .arg(cssRgba(color_constants::timeline_commentary),
                     escapeAndWrapNewlines(commentary)),
            container);
        comment_label->setWordWrap(true);
        QFont comment_font = comment_label->font();
        comment_font.setPointSize(qMax(comment_font.pointSize() - 1, 7));
        comment_font.setItalic(true);
        comment_label->setFont(comment_font);
        item_layout->addWidget(comment_label);
    }

    return container;
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
    emit statusChanged(QString("%1 has new versions - click Reload to see them").arg(code()));
    if (reloadPulse_)
        reloadPulse_->start_pulsing();
}

void HistoryDialogBase::onReloadPulseStateChanged(bool is_on) {
    if (reloadAction_)
        reloadAction_->setIcon(is_on ? reloadStaleIcon() : reloadIcon());
}

void HistoryDialogBase::onCompareToggled(int button_id) {
    onlyChangesMode_ = button_id == 1;
    displayDiffPane();
}

QAbstractItemView* HistoryDialogBase::activeVersionView() const {
    if (widgets_.versionTimeline)
        return widgets_.versionTimeline;
    return widgets_.versionList;
}

void HistoryDialogBase::onCompareComboChanged() {
    displayDiffPane();
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

    if (widgets_.versionTimeline) {
        widgets_.versionTimeline->setSelectionMode(QAbstractItemView::SingleSelection);
        widgets_.versionTimeline->setStyleSheet(
            "QListWidget { border: none; }"
            "QListWidget::item { border-bottom: 1px solid palette(mid); border-left: 3px solid "
            "transparent; }"
            "QListWidget::item:selected { background-color: palette(highlight); border-left: 3px "
            "solid palette(link); }");
        connect(widgets_.versionTimeline,
                &QListWidget::currentRowChanged,
                this,
                &HistoryDialogBase::onVersionSelectedRow);
        connect(widgets_.versionTimeline,
                &QListWidget::itemDoubleClicked,
                this,
                [this](QListWidgetItem*) { onOpenClicked(); });
    } else if (widgets_.versionList) {
        widgets_.versionList->setSelectionMode(QAbstractItemView::SingleSelection);
        connect(widgets_.versionList,
                &QTableWidget::currentCellChanged,
                this,
                [this](int currentRow, int, int, int) { onVersionSelectedRow(currentRow); });
        connect(widgets_.versionList, &QTableWidget::cellDoubleClicked, this, [this](int, int) {
            onOpenClicked();
        });

        widgets_.versionList->setAlternatingRowColors(true);
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

    if (widgets_.compareFromCombo && widgets_.compareToCombo) {
        connect(widgets_.compareFromCombo,
                qOverload<int>(&QComboBox::currentIndexChanged),
                this,
                &HistoryDialogBase::onCompareComboChanged);
        connect(widgets_.compareToCombo,
                qOverload<int>(&QComboBox::currentIndexChanged),
                this,
                &HistoryDialogBase::onCompareComboChanged);
    }

    if (widgets_.allFieldsToggle && widgets_.onlyChangesToggle) {
        // Same segmented two-button pill idiom as the synthetic market
        // data generator's Simple/Advanced toggle, for UX consistency
        // across the app — not a checkbox.
        const QColor accent = palette().color(QPalette::Highlight);
        const QColor accent_text = palette().color(QPalette::HighlightedText);
        const QString seg_style =
            QString("QPushButton { min-height: 26px; min-width: 90px; font-weight: bold; "
                    "padding: 2px 12px; border: 1px solid %1; }"
                    "QPushButton:checked { background: %1; color: %2; }")
                .arg(accent.name(), accent_text.name());
        widgets_.allFieldsToggle->setStyleSheet(
            seg_style +
            "QPushButton { border-top-right-radius: 0; border-bottom-right-radius: 0; }");
        widgets_.onlyChangesToggle->setStyleSheet(
            seg_style + "QPushButton { border-top-left-radius: 0; border-bottom-left-radius: 0; "
                        "border-left: none; }");
        for (auto* b : {widgets_.allFieldsToggle, widgets_.onlyChangesToggle}) {
            b->setCheckable(true);
            b->setAutoExclusive(true);
            b->setCursor(Qt::PointingHandCursor);
        }
        onlyChangesGroup_ = new QButtonGroup(this);
        onlyChangesGroup_->setExclusive(true);
        onlyChangesGroup_->addButton(widgets_.allFieldsToggle, 0);
        onlyChangesGroup_->addButton(widgets_.onlyChangesToggle, 1);
        (onlyChangesMode_ ? widgets_.onlyChangesToggle : widgets_.allFieldsToggle)
            ->setChecked(true);
        connect(onlyChangesGroup_, &QButtonGroup::idClicked, this,
                &HistoryDialogBase::onCompareToggled);
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
    if (mainLayout) {
        mainLayout->insertWidget(0, toolBar_);
        // Legacy dialogs keep a title label above the splitter;
        // compare-mode dialogs drop it (the window title already
        // shows it). Either way, give only the splitter (the version
        // list + diff/details pane) the extra vertical space so
        // resizing the window doesn't leave dead space above it.
        for (int i = 0; i < mainLayout->count(); ++i) {
            const bool is_splitter =
                qobject_cast<QSplitter*>(mainLayout->itemAt(i)->widget()) != nullptr;
            mainLayout->setStretch(i, is_splitter ? 1 : 0);
        }
    }
}

void HistoryDialogBase::historyLoaded() {
    if (!widgets_.versionTimeline && !widgets_.versionList)
        return;

    const int size = historySize();

    if (widgets_.versionTimeline) {
        widgets_.versionTimeline->clear();
        for (int i = 0; i < size; ++i) {
            const VersionRow row = versionRow(i);
            auto* item = new QListWidgetItem(widgets_.versionTimeline);
            auto* item_widget =
                makeTimelineItemWidget(widgets_.versionTimeline, row.version, row.cells);
            item->setSizeHint(item_widget->sizeHint());
            widgets_.versionTimeline->setItemWidget(item, item_widget);
        }
        populateCompareCombos();
        if (size > 0) {
            widgets_.versionTimeline->clearSelection();
            widgets_.versionTimeline->setCurrentRow(0);
            widgets_.versionTimeline->item(0)->setSelected(true);
            if (widgets_.titleLabel)
                widgets_.titleLabel->setText(historyTitle());
        }
    } else {
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
    auto* view = activeVersionView();
    return view ? view->selectionModel()->currentIndex().row() : -1;
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

    if (supportsCompareMode()) {
        // Selecting a version in the timeline previews it against its
        // immediate predecessor by default; the compare combos stay
        // independently adjustable for an arbitrary pair.
        if (widgets_.compareToCombo) {
            const QSignalBlocker blocker(widgets_.compareToCombo);
            widgets_.compareToCombo->setCurrentIndex(widgets_.compareToCombo->findData(row));
        }
        if (widgets_.compareFromCombo) {
            // Compare against the predecessor by default; the oldest
            // version has none, so "From" collapses onto the same
            // version as "To" (displayDiffPane treats index_old ==
            // index_new as "show this version's fields as-is").
            const int from_index = row + 1 < historySize() ? row + 1 : row;
            const QSignalBlocker blocker(widgets_.compareFromCombo);
            widgets_.compareFromCombo->setCurrentIndex(
                widgets_.compareFromCombo->findData(from_index));
        }
        displayDiffPane();
        displayFullDetails(row);
    } else {
        displayChangesTab(row);
        displayFullDetails(row);
    }
    updateActionStates();
}

void HistoryDialogBase::populateCompareCombos() {
    if (!widgets_.compareFromCombo || !widgets_.compareToCombo)
        return;

    const QSignalBlocker from_blocker(widgets_.compareFromCombo);
    const QSignalBlocker to_blocker(widgets_.compareToCombo);

    widgets_.compareFromCombo->clear();
    widgets_.compareToCombo->clear();
    for (int i = 0; i < historySize(); ++i) {
        const QString label = tr("v%1").arg(versionRow(i).version);
        widgets_.compareFromCombo->addItem(label, i);
        widgets_.compareToCombo->addItem(label, i);
    }
    if (historySize() > 0) {
        widgets_.compareToCombo->setCurrentIndex(0);
        widgets_.compareFromCombo->setCurrentIndex(historySize() > 1 ? 1 : 0);
    }
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

    renderDiffRows(diffs);
}

void HistoryDialogBase::displayDiffPane() {
    if (!widgets_.diffBrowser)
        return;

    if (!widgets_.compareFromCombo || !widgets_.compareToCombo ||
        widgets_.compareFromCombo->currentIndex() < 0 ||
        widgets_.compareToCombo->currentIndex() < 0) {
        widgets_.diffBrowser->setHtml(QString());
        return;
    }

    // 0 is newest, so the older ("From") side must be the larger index.
    const int index_old = widgets_.compareFromCombo->currentData().toInt();
    const int index_new = widgets_.compareToCombo->currentData().toInt();

    if (index_old < index_new) {
        widgets_.diffBrowser->setHtml(
            tr("<p>The <b>From</b> version must be older than the <b>To</b> version.</p>"));
        return;
    }

    // Same version on both sides (e.g. the oldest version, which has
    // no predecessor to diff against): there is nothing to compare,
    // so honour the toggle directly rather than reporting an error —
    // "Only Changes" has nothing to show, "All Fields" shows the
    // version's fields as-is.
    if (index_old == index_new) {
        if (!onlyChangesMode_) {
            renderComparePane(calculateDiffBetween(index_new, index_old, true));
        } else {
            widgets_.diffBrowser->setHtml(tr("<p>(No field changes)</p>"));
        }
        return;
    }

    const DiffResult diffs = calculateDiffBetween(index_new, index_old, !onlyChangesMode_);
    if (diffs.isEmpty()) {
        widgets_.diffBrowser->setHtml(tr("<p>(No field changes)</p>"));
        return;
    }

    renderComparePane(diffs);
}

void HistoryDialogBase::renderDiffRows(const DiffResult& diffs) {
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
        // An unchanged field (old_val == new_val, only possible when
        // include_unchanged requested it) yields no compute() entry —
        // both sides then fall through to the plain, uncoloured text
        // path below with identical values, which is exactly the
        // desired "All Fields" context-row rendering.
        using ores::diff::domain::field_value;
        const auto field_std = field.toStdString();
        const auto changes =
            ores::diff::engine::compute({field_value{field_std, old_val.toStdString()}},
                                        {field_value{field_std, new_val.toStdString()}});
        const auto* entry = changes.entries.empty() ? nullptr : &changes.entries.front();

        if (QWidget* old_widget = changeCellWidget(field, old_val)) {
            widgets_.changesTable->setCellWidget(i, 1, old_widget);
        } else if (entry) {
            widgets_.changesTable->setCellWidget(
                i,
                1,
                makeDiffLabel(widgets_.changesTable,
                              renderDiffCell(entry->old_value,
                                             entry->old_spans,
                                             color_constants::diff_old_line_bg,
                                             color_constants::diff_old_span_bg)));
        } else {
            widgets_.changesTable->setItem(i, 1, new QTableWidgetItem(old_val));
        }

        if (QWidget* new_widget = changeCellWidget(field, new_val)) {
            widgets_.changesTable->setCellWidget(i, 2, new_widget);
        } else if (entry) {
            widgets_.changesTable->setCellWidget(
                i,
                2,
                makeDiffLabel(widgets_.changesTable,
                              renderDiffCell(entry->new_value,
                                             entry->new_spans,
                                             color_constants::diff_new_line_bg,
                                             color_constants::diff_new_span_bg)));
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

void HistoryDialogBase::renderComparePane(const DiffResult& diffs) {
    // Rendered as one HTML document in a QTextBrowser rather than a
    // per-row QTableWidget: a QTextDocument lays out and wraps HTML
    // table cells (including multiline content) correctly on its own,
    // sidestepping the whole class of manual-row-height bugs a
    // QTableWidget + cell-widget approach runs into for long values.
    QString html = "<style>"
                   "table { width: 100%; border-collapse: collapse; }"
                   "td, th { padding: 6px; vertical-align: top; text-align: left; }"
                   "th { border-bottom: 2px solid palette(mid); }"
                   "td { border-bottom: 1px solid palette(mid); }"
                   "</style><table><tr><th width=\"25%\">Field</th><th>Value Diff</th></tr>";

    for (const auto& [field, values] : diffs) {
        const auto& [old_val, new_val] = values;

        using ores::diff::domain::field_value;
        const auto field_std = field.toStdString();
        const auto changes =
            ores::diff::engine::compute({field_value{field_std, old_val.toStdString()}},
                                        {field_value{field_std, new_val.toStdString()}});
        const auto* entry = changes.entries.empty() ? nullptr : &changes.entries.front();

        QString value_html;
        if (entry) {
            value_html = renderDiffCell(entry->old_value, entry->old_spans,
                                       color_constants::diff_old_line_bg,
                                       color_constants::diff_old_span_bg) +
                        renderDiffCell(entry->new_value, entry->new_spans,
                                       color_constants::diff_new_line_bg,
                                       color_constants::diff_new_span_bg);
        } else {
            // Unchanged field (All Fields mode): show the value once,
            // uncoloured — old and new are identical.
            value_html = QString("<div style=\"padding:2px;\">%1</div>")
                             .arg(escapeAndWrapNewlines(old_val));
        }
        html += QString("<tr><td><b>%1</b></td><td>%2</td></tr>")
                    .arg(field.toHtmlEscaped(), value_html);
    }
    html += "</table>";

    widgets_.diffBrowser->setHtml(html);
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
