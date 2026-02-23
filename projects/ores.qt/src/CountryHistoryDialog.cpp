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
#include "ores.qt/CountryHistoryDialog.hpp"

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
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

const QIcon& CountryHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon = IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor);
    return historyIcon;
}

CountryHistoryDialog::CountryHistoryDialog(QString alpha2_code,
    ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), ui_(new Ui::CountryHistoryDialog),
      clientManager_(clientManager), imageCache_(nullptr),
      alpha2Code_(std::move(alpha2_code)),
      toolBar_(nullptr), reloadAction_(nullptr),
      openAction_(nullptr), revertAction_(nullptr) {

    BOOST_LOG_SEV(lg(), info) << "Creating country history widget for: "
                              << alpha2Code_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    setupToolbar();

    connect(ui_->versionListWidget, &QTableWidget::currentCellChanged,
            this, [this](int currentRow, int, int, int) {
        onVersionSelected(currentRow);
    });

    // Double-click opens the version in read-only mode
    connect(ui_->versionListWidget, &QTableWidget::cellDoubleClicked,
            this, [this](int, int) {
        onOpenClicked();
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

CountryHistoryDialog::~CountryHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying country history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CountryHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading country history for: "
                              << alpha2Code_.toStdString();

    refdata::messaging::get_country_history_request request{alpha2Code_.toStdString()};
    auto payload = request.serialize();

    frame request_frame(message_type::get_country_history_request,
        0, std::move(payload)
    );

    using HistoryResult = std::expected<frame, std::string>;
    QPointer<CountryHistoryDialog> self = this;
    QFuture<HistoryResult> future =
        QtConcurrent::run([self, frame = std::move(request_frame)]() mutable -> HistoryResult {
        if (!self->clientManager_ || !self->clientManager_->isConnected()) {
             return std::unexpected("Disconnected from server");
        }
        auto response_result = self->clientManager_->sendRequest(std::move(frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Could not obtain country history: "
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
        if (result->header().type != message_type::get_country_history_response) {
            self->onHistoryLoadError(
                QString("Server does not support country history: received message type %1")
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

        auto response = refdata::messaging::get_country_history_response::
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

void CountryHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(history_.size());

    for (int i = 0; i < static_cast<int>(history_.size()); ++i) {
        const auto& country = history_[i];

        BOOST_LOG_SEV(lg(), trace) << "Displaying version [" << i << "]: "
                                   << "version=" << country.version
                                   << ", modified_by=" << country.modified_by;

        auto* versionItem =
            new QTableWidgetItem(QString::number(country.version));
        auto* recordedAtItem =
            new QTableWidgetItem(relative_time_helper::format(country.recorded_at));
        auto* modifiedByItem =
            new QTableWidgetItem(QString::fromStdString(country.modified_by));
        auto* changeReasonItem =
            new QTableWidgetItem(QString::fromStdString(country.change_reason_code));
        auto* commentaryItem =
            new QTableWidgetItem(QString::fromStdString(country.change_commentary));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, recordedAtItem);
        ui_->versionListWidget->setItem(i, 2, modifiedByItem);
        ui_->versionListWidget->setItem(i, 3, changeReasonItem);
        ui_->versionListWidget->setItem(i, 4, commentaryItem);
    }

    if (!history_.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.empty()) {
        const auto& latest = history_[0];
        ui_->titleLabel->setText(QString("Country History: %1 - %2")
            .arg(alpha2Code_)
            .arg(QString::fromStdString(latest.name)));
    }

    updateButtonStates();

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.size()));
}

void CountryHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load country history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load country history:\n%1")
        .arg(error_msg));
}

void CountryHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void CountryHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& current = history_[version_index];

    // If this is the first (oldest) version, there's nothing to diff against so
    // leave the changes table empty
    if (version_index == static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), trace) << "No previous version to diff against for oldest version";
        return;
    }

    // Calculate diff with previous version
    const auto& previous = history_[version_index + 1];

    BOOST_LOG_SEV(lg(), trace) << "Calculating diff between version "
                               << current.version << " and " << previous.version;
    BOOST_LOG_SEV(lg(), trace) << "Current name: " << current.name
                               << ", Previous name: " << previous.name;

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
                    QIcon icon = imageCache_->getIcon(imageIdStr.toStdString());
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

void CountryHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.size()))
        return;

    const auto& country = history_[version_index];

    ui_->alpha2CodeValue->setText(QString::fromStdString(country.alpha2_code));
    ui_->alpha3CodeValue->setText(QString::fromStdString(country.alpha3_code));
    ui_->numericCodeValue->setText(QString::fromStdString(country.numeric_code));
    ui_->nameValue->setText(QString::fromStdString(country.name));
    ui_->officialNameValue->setText(QString::fromStdString(country.official_name));
    ui_->versionNumberValue->setText(QString::number(country.version));
    ui_->modifiedByValue->setText(QString::fromStdString(country.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(country.recorded_at));
}

CountryHistoryDialog::DiffResult CountryHistoryDialog::
calculateDiff(const refdata::domain::country& current,
    const refdata::domain::country& previous) {

    DiffResult diffs;

    // Helper to check string field differences
    auto checkDiffString = [&diffs](const QString& fieldName,
        const std::string& currentVal, const std::string& previousVal) {
        if (currentVal != previousVal) {
            diffs.append({fieldName, {QString::fromStdString(previousVal),
                                      QString::fromStdString(currentVal)}});
        }
    };

    // Compare string fields
    checkDiffString("Alpha-2 Code", current.alpha2_code, previous.alpha2_code);
    checkDiffString("Alpha-3 Code", current.alpha3_code, previous.alpha3_code);
    checkDiffString("Numeric Code", current.numeric_code, previous.numeric_code);
    checkDiffString("Name", current.name, previous.name);
    checkDiffString("Official Name", current.official_name, previous.official_name);

    // Compare change management fields
    checkDiffString("Change Reason", current.change_reason_code, previous.change_reason_code);
    checkDiffString("Commentary", current.change_commentary, previous.change_commentary);

    // Compare image_id (flag)
    if (current.image_id != previous.image_id) {
        auto formatImageId = [](const std::optional<boost::uuids::uuid>& id) {
            if (!id.has_value()) {
                return QString("(none)");
            }
            return QString::fromStdString(boost::uuids::to_string(*id));
        };
        diffs.append({"Flag", {formatImageId(previous.image_id),
                               formatImageId(current.image_id)}});
    }

    return diffs;
}

void CountryHistoryDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Reload action
    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    reloadAction_->setToolTip("Reload history from server");
    connect(reloadAction_, &QAction::triggered, this,
        &CountryHistoryDialog::onReloadClicked);
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    // Create Open action
    openAction_ = new QAction("Open", this);
    openAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Edit, IconUtils::DefaultIconColor));
    openAction_->setToolTip("Open this version in read-only mode");
    connect(openAction_, &QAction::triggered, this,
        &CountryHistoryDialog::onOpenClicked);
    toolBar_->addAction(openAction_);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert country to this version");
    connect(revertAction_, &QAction::triggered, this,
        &CountryHistoryDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);

    // Add toolbar to layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void CountryHistoryDialog::updateButtonStates() {
    const int index = selectedVersionIndex();
    const bool hasSelection = index >= 0 &&
        index < static_cast<int>(history_.size());

    if (openAction_)
        openAction_->setEnabled(hasSelection);

    if (revertAction_)
        revertAction_->setEnabled(hasSelection);
}

int CountryHistoryDialog::selectedVersionIndex() const {
    return ui_->versionListWidget->currentRow();
}

void CountryHistoryDialog::onOpenClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    const auto& country = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening country version "
                              << country.version << " in read-only mode";

    emit openVersionRequested(country, country.version);
}

void CountryHistoryDialog::onRevertClicked() {
    const int index = selectedVersionIndex();
    if (index < 0 || index >= static_cast<int>(history_.size()))
        return;

    // Get the selected version and the previous (older) version
    const auto& current = history_[index];

    // If this is the oldest version, there's no previous version to revert to
    if (index == static_cast<int>(history_.size()) - 1) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot revert oldest version - no previous version exists";
        MessageBoxHelper::information(this, "Cannot Revert",
            "This is the oldest version. There is no previous version to revert to.");
        return;
    }

    // The "previous" version is the one we want to revert TO (the "old" side in the diff)
    const auto& previous = history_[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert from version "
                              << current.version << " to version "
                              << previous.version;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Country",
        QString("Are you sure you want to revert '%1' from version %2 back to version %3?\n\n"
                "This will create a new version with the data from version %3.")
            .arg(alpha2Code_)
            .arg(current.version)
            .arg(previous.version),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    // Use the PREVIOUS version's data (the "old" side of the diff) with the latest version number
    refdata::domain::country countryToRevert = previous;
    countryToRevert.version = history_[0].version;
    emit revertVersionRequested(countryToRevert);
}

void CountryHistoryDialog::onReloadClicked() {
    BOOST_LOG_SEV(lg(), info) << "Reload requested for country history: "
                              << alpha2Code_.toStdString();
    emit statusChanged(QString("Reloading history for %1...").arg(alpha2Code_));
    loadHistory();
}

QSize CountryHistoryDialog::sizeHint() const {
    QSize baseSize = QWidget::sizeHint();

    const int minimumWidth = 900;
    const int minimumHeight = 600;

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void CountryHistoryDialog::markAsStale() {
    BOOST_LOG_SEV(lg(), info) << "Country history marked as stale for: "
                              << alpha2Code_.toStdString() << ", reloading...";

    emit statusChanged(QString("Country %1 was modified - reloading history...")
        .arg(alpha2Code_));

    // Reload history data
    loadHistory();
}

void CountryHistoryDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

}
