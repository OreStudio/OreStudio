/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CurrencyHistoryDialog.hpp"

#include <QIcon>
#include <QLabel>
#include <QDateTime>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::telemetry::log;

const QIcon& CurrencyHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon(":/icons/ic_fluent_history_20_regular.svg");
    return historyIcon;
}

CurrencyHistoryDialog::CurrencyHistoryDialog(QString iso_code,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyHistoryDialog),
      clientManager_(clientManager), imageCache_(nullptr),
      isoCode_(std::move(iso_code)),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency history widget for: "
                              << isoCode_.toStdString();

    ui_->setupUi(this);

    setupToolbar();

    connect(ui_->versionListWidget, &QTableWidget::currentCellChanged,
            this, [this](int currentRow, int, int, int) {
        onVersionSelected(currentRow);
    });

    ui_->versionListWidget->setAlternatingRowColors(true);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->resizeRowsToContents();

    QHeaderView* versionVerticalHeader = ui_->versionListWidget->verticalHeader();
    QHeaderView* versionHorizontalHeader = ui_->versionListWidget->horizontalHeader();
    versionVerticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    versionHorizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->changesTableWidget->setColumnWidth(0, 200);
    ui_->changesTableWidget->setColumnWidth(1, 200);

    updateButtonStates();
}

CurrencyHistoryDialog::~CurrencyHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying currency history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading currency history for: "
                              << isoCode_.toStdString();

    risk::messaging::get_currency_history_request request{isoCode_.toStdString()};
    auto payload = request.serialize();

    frame request_frame(message_type::get_currency_history_request,
        0, std::move(payload)
    );

    using HistoryResult = std::expected<frame, std::string>;
    QPointer<CurrencyHistoryDialog> self = this;
    QFuture<HistoryResult> future =
        QtConcurrent::run([self, frame = std::move(request_frame)]() mutable -> HistoryResult {
        if (!self->clientManager_ || !self->clientManager_->isConnected()) {
             return std::unexpected("Disconnected from server");
        }
        auto response_result = self->clientManager_->sendRequest(std::move(frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Could not obtain currency history: "
                                       << "Failed to communicate with server.";
            return std::unexpected("Failed to communicate with server");
        }
        return *response_result;
    });

    // Use watcher to handle results
    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished, self,
        [self, watcher]() {

        if (!self) return;
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result) {
            self->onHistoryLoadError(QString::fromStdString(result.error()));
            return;
        }

        // Check if server sent an error_response instead
        if (result->header().type != message_type::get_currency_history_response) {
            self->onHistoryLoadError(
                QString("Server does not support currency history: received message type %1")
                .arg(static_cast<int>(result->header().type)));
            return;
        }

        // Decompress payload
        auto payload_result = result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress history response";
            self->onHistoryLoadError("Failed to decompress server response");
            return;
        }

        auto response = risk::messaging::get_currency_history_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Could not deserialise server response.";
            self->onHistoryLoadError("Invalid server response");
            return;
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            self->onHistoryLoadError(QString::fromStdString(response->message));
            return;
        }

        self->history_ = std::move(response->history);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void CurrencyHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.versions.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(history_.versions.size());

    for (int i = 0; i < static_cast<int>(history_.versions.size()); ++i) {
        const auto& version = history_.versions[i];

        BOOST_LOG_SEV(lg(), trace) << "Displaying version [" << i << "]: "
                                   << "version_number=" << version.version_number
                                   << ", data.version=" << version.data.version
                                   << ", recorded_by=" << version.recorded_by;

        auto* versionItem =
            new QTableWidgetItem(QString::number(version.version_number));
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(version.recorded_at));
        auto* recordedByItem =
            new QTableWidgetItem(QString::fromStdString(version.recorded_by));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, recordedAtItem);
        ui_->versionListWidget->setItem(i, 2, recordedByItem);
    }

    if (!history_.versions.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.versions.empty()) {
        const auto& latest = history_.versions[0];
        ui_->titleLabel->setText(QString("Currency History: %1 - %2")
            .arg(isoCode_)
            .arg(QString::fromStdString(latest.data.name)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.versions.size()));
}

void CurrencyHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load currency history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load currency history:\n%1")
        .arg(error_msg));
}

void CurrencyHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.versions.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void CurrencyHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.versions.size()))
        return;

    const auto& current = history_.versions[version_index];

    // If this is the first (oldest) version, there's nothing to diff against so
    // leave the changes table empty
    if (version_index == static_cast<int>(history_.versions.size()) - 1) {
        BOOST_LOG_SEV(lg(), trace) << "No previous version to diff against for oldest version";
        return;
    }

    // Calculate diff with previous version
    const auto& previous = history_.versions[version_index + 1];

    BOOST_LOG_SEV(lg(), trace) << "Calculating diff between version "
                               << current.version_number << " and " << previous.version_number;
    BOOST_LOG_SEV(lg(), trace) << "Current name: " << current.data.name
                               << ", Previous name: " << previous.data.name;

    auto diffs = calculateDiff(current, previous);

    BOOST_LOG_SEV(lg(), trace) << "Found " << diffs.size() << " differences";

    ui_->changesTableWidget->setRowCount(diffs.size());

    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        auto* fieldItem = new QTableWidgetItem(field);
        ui_->changesTableWidget->setItem(i, 0, fieldItem);

        // Special handling for Flag field - show SVG icons instead of UUIDs
        if (field == "Flag" && imageCache_) {
            // Create label widgets to show the flag icons
            auto createFlagLabel = [this](const QString& imageIdStr) -> QWidget* {
                auto* label = new QLabel();
                label->setAlignment(Qt::AlignCenter);
                label->setFixedSize(32, 32);

                if (imageIdStr == "(none)") {
                    QIcon noFlagIcon = imageCache_->getNoFlagIcon();
                    if (!noFlagIcon.isNull()) {
                        label->setPixmap(noFlagIcon.pixmap(24, 24));
                    } else {
                        label->setText("-");
                    }
                } else {
                    // Get the icon using the UUID string
                    QIcon icon = imageCache_->getImageIcon(imageIdStr.toStdString());
                    if (!icon.isNull()) {
                        label->setPixmap(icon.pixmap(24, 24));
                    } else {
                        label->setText("?");
                        label->setToolTip(imageIdStr);
                    }
                }
                return label;
            };

            ui_->changesTableWidget->setCellWidget(i, 1, createFlagLabel(old_val));
            ui_->changesTableWidget->setCellWidget(i, 2, createFlagLabel(new_val));
        } else {
            auto* oldItem = new QTableWidgetItem(old_val);
            auto* newItem = new QTableWidgetItem(new_val);
            ui_->changesTableWidget->setItem(i, 1, oldItem);
            ui_->changesTableWidget->setItem(i, 2, newItem);
        }
    }
}

void CurrencyHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.versions.size()))
        return;

    const auto& version = history_.versions[version_index];
    const auto& data = version.data;

    ui_->isoCodeValue->setText(QString::fromStdString(data.iso_code));
    ui_->nameValue->setText(QString::fromStdString(data.name));
    ui_->numericCodeValue->setText(QString::fromStdString(data.numeric_code));
    ui_->symbolValue->setText(QString::fromStdString(data.symbol));
    ui_->fractionSymbolValue->setText(QString::fromStdString(data.fraction_symbol));
    ui_->fractionsPerUnitValue->setText(QString::number(data.fractions_per_unit));
    ui_->versionNumberValue->setText(QString::number(version.version_number));
    ui_->recordedByValue->setText(QString::fromStdString(version.recorded_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
}

#define CHECK_DIFF_STRING(FIELD_NAME, FIELD) \
    if (current.data.FIELD != previous.data.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::fromStdString(previous.data.FIELD), \
            QString::fromStdString(current.data.FIELD) \
        }}); \
    }

#define CHECK_DIFF_INT(FIELD_NAME, FIELD) \
    if (current.data.FIELD != previous.data.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::number(previous.data.FIELD), \
            QString::number(current.data.FIELD) \
        }}); \
    }

CurrencyHistoryDialog::DiffResult CurrencyHistoryDialog::
calculateDiff(const risk::domain::currency_version& current,
    const risk::domain::currency_version& previous) {

    DiffResult diffs;

    // Compare string fields
    CHECK_DIFF_STRING("ISO Code", iso_code);
    CHECK_DIFF_STRING("Name", name);
    CHECK_DIFF_STRING("Numeric Code", numeric_code);
    CHECK_DIFF_STRING("Symbol", symbol);
    CHECK_DIFF_STRING("Fraction Symbol", fraction_symbol);
    CHECK_DIFF_STRING("Rounding Type", rounding_type);
    CHECK_DIFF_STRING("Format", format);
    CHECK_DIFF_STRING("Currency Type", currency_type);

    // Compare integer fields
    CHECK_DIFF_INT("Fractions Per Unit", fractions_per_unit);
    CHECK_DIFF_INT("Rounding Precision", rounding_precision);

    // Compare image_id (flag)
    if (current.data.image_id != previous.data.image_id) {
        auto formatImageId = [](const std::optional<boost::uuids::uuid>& id) {
            if (!id.has_value()) {
                return QString("(none)");
            }
            return QString::fromStdString(boost::uuids::to_string(*id));
        };
        diffs.append({"Flag", {formatImageId(previous.data.image_id),
                               formatImageId(current.data.image_id)}});
    }

    return diffs;
}

#undef CHECK_DIFF_STRING
#undef CHECK_DIFF_INT

void CurrencyHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    const QColor iconColor(220, 220, 220);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &CurrencyHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &CurrencyHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));
    revertAction_->setToolTip("Revert currency to this version");
    connect(revertAction_, &QAction::triggered, this,
        &CurrencyHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void CurrencyHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(history_.versions.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int CurrencyHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void CurrencyHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.versions.size()))
        return;

    const auto& version = history_.versions[index];
    BOOST_LOG_SEV(lg(), info) << "Opening currency version "
                              << version.version_number << " in read-only mode";

    emit openVersionRequested(version.data, version.version_number);
}

void CurrencyHistoryDialog::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.versions.size()))
        return;

    // Get the selected version and the previous (older) version
    const auto& current = history_.versions[index];

    // If this is the oldest version, there's no previous version to revert to
    if (index == static_cast<int>(history_.versions.size()) - 1) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot revert oldest version - no previous version exists";
        MessageBoxHelper::information(this, "Cannot Revert",
            "This is the oldest version. There is no previous version to revert to.");
        return;
    }

    // The "previous" version is the one we want to revert TO (the "old" side in the diff)
    const auto& previous = history_.versions[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert from version "
                              << current.version_number << " to version "
                              << previous.version_number;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Currency",
        QString("Are you sure you want to revert '%1' from version %2 back to version %3?\n\n"
                "This will create a new version with the data from version %3.")
            .arg(isoCode_)
            .arg(current.version_number)
            .arg(previous.version_number),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    // Use the PREVIOUS version's data (the "old" side of the diff) with the latest version number
    risk::domain::currency currency = previous.data;
    currency.version = history_.versions[0].version_number;
    emit revertVersionRequested(currency);
}

void CurrencyHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for currency history: "
                              << isoCode_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(isoCode_));
    loadHistory();
}

QSize CurrencyHistoryDialog::sizeHint() const {
    // Call the base implementation first to get the minimum size required by
    // the layout manager and its content's size policies.
    QSize baseSize = QWidget::sizeHint();

    // Define a reasonable minimum size for a history dialog. This ensures the
    // two panes (Version List and Details/Changes) are comfortably visible.
    // These numbers are chosen to fit most content without excessive manual
    // calculation.
    const int minimumWidth = 900;
    const int minimumHeight = 600;

    // Return the maximum of the base size (to accommodate large text/UI
    // elements) and the defined minimum size.
    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void CurrencyHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "Currency history marked as stale for: "
                              << isoCode_.toStdString() << ", reloading...";

    emit statusChanged(QString("Currency %1 was modified - reloading history...")
        .arg(isoCode_));

    // Reload history data
    loadHistory();
}

void CurrencyHistoryDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

}
