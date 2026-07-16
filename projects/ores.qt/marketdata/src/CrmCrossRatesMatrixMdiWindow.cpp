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
#include "ores.qt/CrmCrossRatesMatrixMdiWindow.hpp"
#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include "ores.ore.core/market/market_datum.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/CrmRateCellWidget.hpp"
#include "ores.qt/CrmRateSparklineWidget.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QDateTime>
#include <QDesktopServices>
#include <QFile>
#include <QFileDialog>
#include <QFrame>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QMessageBox>
#include <QSet>
#include <QSettings>
#include <QUrl>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <map>
#include <sstream>

namespace ores::qt {

using namespace ores::logging;
namespace marketdata_msg = ores::marketdata::messaging;

namespace {

constexpr int cell_width = 110;
constexpr int cell_height = 56;
constexpr int row_header_width = 100;
constexpr std::size_t max_history_points = 30;

QTableWidgetItem* make_dash_item() {
    auto* item = new QTableWidgetItem(QStringLiteral("-"));
    item->setTextAlignment(Qt::AlignCenter);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

struct RatesResult {
    bool success{false};
    QString error;
    std::vector<marketdata_msg::crm_rate_item> rates;
};

/// Parses the "YYYY-MM-DD" prefix of an ISO-8601 as_of string (e.g.
/// "2026-07-15T09:34:00Z") into a year_month_day for market_datum::date.
/// Returns today's date (in the absence of a more meaningful fallback --
/// this is only ever reached for a rate that's already fresh/stale, i.e.
/// as_of is never empty here) if the string is malformed.
std::chrono::year_month_day parse_as_of_date(const std::string& as_of) {
    if (as_of.size() >= 10) {
        try {
            const int year = std::stoi(as_of.substr(0, 4));
            const unsigned month = static_cast<unsigned>(std::stoi(as_of.substr(5, 2)));
            const unsigned day = static_cast<unsigned>(std::stoi(as_of.substr(8, 2)));
            return std::chrono::year_month_day{
                std::chrono::year{year}, std::chrono::month{month}, std::chrono::day{day}};
        } catch (const std::exception&) {
            // fall through to today's date below
        }
    }
    const auto today = std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());
    return std::chrono::year_month_day{today};
}

/// Compact ISO-8601 UTC timestamp (no colons -- filesystem-safe) for
/// default export filenames, e.g. "20260715T131530Z".
QString export_timestamp() {
    return QDateTime::currentDateTimeUtc().toString(QStringLiteral("yyyyMMdd'T'HHmmss'Z'"));
}

} // namespace

CrmCrossRatesMatrixMdiWindow::CrmCrossRatesMatrixMdiWindow(ClientManager* clientManager,
                                                           ImageCache* imageCache,
                                                           QString crmName,
                                                           QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , imageCache_(imageCache)
    , crmName_(std::move(crmName))
    , settingsGroup_(QStringLiteral("CrmCrossRatesMatrixWindow_") +
                     (crmName_.isEmpty() ? QStringLiteral("All") : crmName_))
    , toolbar_(nullptr)
    , baseCurrencyCombo_(nullptr)
    , refreshIntervalCombo_(nullptr)
    , reloadAction_(nullptr)
    , hideEmptyButton_(nullptr)
    , showInvertedButton_(nullptr)
    , autoRefreshTimer_(nullptr)
    , table_(nullptr)
    , footerLabel_(nullptr)
    , overviewPairLabel_(nullptr)
    , overviewRateLabel_(nullptr)
    , overviewSparkline_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    connect(autoRefreshTimer_, &QTimer::timeout, this, &CrmCrossRatesMatrixMdiWindow::reload);

    if (clientManager_) {
        // Snapshot the token synchronously on the GUI thread rather than
        // handing the cache a token_provider that would re-read
        // ClientManager's session state from load()'s own background
        // thread later (a real data race against the GUI thread's
        // proactive token-refresh timer) -- safe here specifically
        // because load() only ever fires this once, at construction, so
        // there is no "later" call that would need a fresher token.
        const auto authToken = clientManager_->currentAuthToken();
        conventionCache_ =
            std::make_shared<ores::refdata::service::cache::currency_pair_convention_cache>(
                clientManager_->nats_client(), [authToken](bool /*force*/) { return authToken; });
        // load() is a blocking NATS round trip -- run it off the UI
        // thread, but surface a failure visibly rather than only
        // logging it: nothing reads from conventionCache_ yet (that's
        // the sibling formatter task), so a silent failure here would
        // otherwise be completely invisible, including to a developer
        // testing this live. A QMessageBox rather than statusChanged/
        // errorOccurred (which route into the same transient status-bar
        // channel this window's own 5s auto-refresh success message
        // keeps overwriting) -- load() only ever fires once, at
        // construction, so it needs a notification that doesn't get
        // stomped by the very next routine reload a few seconds later.
        QPointer<CrmCrossRatesMatrixMdiWindow> self = this;
        const auto tenantId = clientManager_->currentTenantId();
        auto* cacheWatcher = new QFutureWatcher<QString>(this);
        connect(cacheWatcher, &QFutureWatcher<QString>::finished, this, [self, cacheWatcher]() {
            const auto error = cacheWatcher->result();
            cacheWatcher->deleteLater();
            if (!self || error.isEmpty())
                return;
            BOOST_LOG_SEV(lg(), warn)
                << "Currency pair convention cache load failed: " << error.toStdString();
            MessageBoxHelper::critical(
                self,
                tr("FX Pair Conventions"),
                tr("Could not load FX pair conventions. Rate display will use default "
                   "formatting until this is resolved."),
                error);
        });
        cacheWatcher->setFuture(QtConcurrent::run([self, tenantId]() -> QString {
            if (!self || !self->conventionCache_)
                return {};
            return QString::fromStdString(self->conventionCache_->load(tenantId));
        }));
    }

    setupUi();
    reload();
}

void CrmCrossRatesMatrixMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(4, 4, 4, 4);
    mainLayout->setSpacing(4);

    setupToolbar();
    mainLayout->addWidget(toolbar_);

    auto* bodyLayout = new QHBoxLayout();

    table_ = new QTableWidget(this);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setSelectionBehavior(QAbstractItemView::SelectItems);
    table_->setAlternatingRowColors(true);
    // Fixed cell/header sizes -- deliberately not Stretch: the grid must
    // not visibly grow/shrink per cell as the currency count changes
    // between reloads (e.g. switching CRM selector), matching the
    // reference mockup's uniform cell sizing.
    table_->horizontalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    table_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    table_->horizontalHeader()->setDefaultSectionSize(cell_width);
    table_->verticalHeader()->setDefaultSectionSize(cell_height);
    // Row-header column (flag + code) needs more width than the header's
    // own content-based size hint gives it by default -- it was visibly
    // squished against the flag icon.
    table_->verticalHeader()->setFixedWidth(row_header_width);
    table_->setIconSize(QSize(20, 14));
    bodyLayout->addWidget(table_, 1);

    // "FX Pair Overview" side panel, matching the reference mockup --
    // shows whichever cell was last clicked, sourced entirely from this
    // panel's own local reload history (no separate rate-history endpoint
    // exists, by design).
    auto* overviewFrame = new QFrame(this);
    overviewFrame->setFrameShape(QFrame::StyledPanel);
    overviewFrame->setFixedWidth(200);
    auto* overviewLayout = new QVBoxLayout(overviewFrame);

    auto* overviewTitle = new QLabel(tr("FX Pair Overview"), overviewFrame);
    overviewTitle->setStyleSheet(QStringLiteral("font-weight: bold;"));
    overviewLayout->addWidget(overviewTitle);

    overviewPairLabel_ = new QLabel(tr("(select a cell)"), overviewFrame);
    overviewLayout->addWidget(overviewPairLabel_);

    overviewRateLabel_ = new QLabel(overviewFrame);
    overviewLayout->addWidget(overviewRateLabel_);

    overviewSparkline_ = new CrmRateSparklineWidget(overviewFrame);
    overviewLayout->addWidget(overviewSparkline_);
    overviewLayout->addStretch(1);

    bodyLayout->addWidget(overviewFrame);
    mainLayout->addLayout(bodyLayout, 1);

    auto* footerLayout = new QHBoxLayout();
    footerLayout->addWidget(new QLabel(tr("Base Currency:"), this));
    baseCurrencyCombo_ = new QComboBox(this);
    baseCurrencyCombo_->setMinimumWidth(120);
    baseCurrencyCombo_->addItem(tr("All"), QString());
    footerLayout->addWidget(baseCurrencyCombo_);
    connect(baseCurrencyCombo_,
            &QComboBox::currentIndexChanged,
            this,
            &CrmCrossRatesMatrixMdiWindow::reload);

    footerLayout->addSpacing(12);
    footerLayout->addWidget(new QLabel(tr("Update Interval:"), this));
    refreshIntervalCombo_ = new QComboBox(this);
    refreshIntervalCombo_->setMinimumWidth(120);
    refreshIntervalCombo_->addItem(tr("No auto-refresh"), 0);
    refreshIntervalCombo_->addItem(tr("5s"), 5);
    refreshIntervalCombo_->addItem(tr("10s"), 10);
    refreshIntervalCombo_->addItem(tr("30s"), 30);
    refreshIntervalCombo_->addItem(tr("60s"), 60);
    // 5s default, matching the reference mockup -- auto-refresh is now
    // the default experience for this panel, not an opt-in extra. See
    // the story's `* Decisions` for why polling (as opposed to
    // server-side broadcast) is fine here.
    refreshIntervalCombo_->setCurrentIndex(1);
    footerLayout->addWidget(refreshIntervalCombo_);
    connect(refreshIntervalCombo_,
            &QComboBox::currentIndexChanged,
            this,
            &CrmCrossRatesMatrixMdiWindow::onRefreshIntervalChanged);

    footerLayout->addSpacing(12);
    // Real checkable toggle button (not a toolbar icon/checkbox) -- hides
    // currencies with no configured pair at all (never appear as base or
    // quote anywhere in the current selection). Distinct from the
    // per-cell dash, which already covers "this specific pair has no
    // data" while the currency itself still has data elsewhere.
    hideEmptyButton_ = new QPushButton(tr("Hide Empty"), this);
    hideEmptyButton_->setCheckable(true);
    hideEmptyButton_->setCursor(Qt::PointingHandCursor);
    hideEmptyButton_->setToolTip(tr("Hide currencies with no configured pair at all"));
    {
        const QColor accent = palette().color(QPalette::Highlight);
        const QColor accentText = palette().color(QPalette::HighlightedText);
        hideEmptyButton_->setStyleSheet(
            QStringLiteral("QPushButton { padding: 3px 10px; border: 1px solid %1; "
                           "border-radius: 3px; } "
                           "QPushButton:checked { background: %1; color: %2; }")
                .arg(accent.name(), accentText.name()));
    }
    footerLayout->addWidget(hideEmptyButton_);
    {
        QSettings settings(QStringLiteral("OreStudio"), QStringLiteral("OreStudio"));
        settings.beginGroup(settingsGroup_);
        hideEmptyButton_->setChecked(settings.value(QStringLiteral("hideEmpty"), false).toBool());
    }
    connect(hideEmptyButton_,
            &QPushButton::toggled,
            this,
            &CrmCrossRatesMatrixMdiWindow::onHideEmptyToggled);

    footerLayout->addSpacing(12);
    // When on, a cell with no direct quote but whose inverse pair *does*
    // have one (e.g. no CAD/EUR row entry, but EUR/CAD exists) shows the
    // computed 1/rate instead of a dash -- pure arithmetic, no new data
    // needed. Off by default: still opt-in, since an inverted rate is a
    // derived display value, not something the CRM actually quotes.
    showInvertedButton_ = new QPushButton(tr("Show Inverted"), this);
    showInvertedButton_->setCheckable(true);
    showInvertedButton_->setCursor(Qt::PointingHandCursor);
    showInvertedButton_->setToolTip(
        tr("Show computed inverse rates (1/rate) for pairs with no direct quote"));
    {
        const QColor accent = palette().color(QPalette::Highlight);
        const QColor accentText = palette().color(QPalette::HighlightedText);
        showInvertedButton_->setStyleSheet(
            QStringLiteral("QPushButton { padding: 3px 10px; border: 1px solid %1; "
                           "border-radius: 3px; } "
                           "QPushButton:checked { background: %1; color: %2; }")
                .arg(accent.name(), accentText.name()));
    }
    footerLayout->addWidget(showInvertedButton_);
    {
        QSettings settings(QStringLiteral("OreStudio"), QStringLiteral("OreStudio"));
        settings.beginGroup(settingsGroup_);
        showInvertedButton_->setChecked(
            settings.value(QStringLiteral("showInverted"), false).toBool());
    }
    connect(showInvertedButton_,
            &QPushButton::toggled,
            this,
            &CrmCrossRatesMatrixMdiWindow::onShowInvertedToggled);

    footerLayout->addStretch(1);

    footerLabel_ = new QLabel(this);
    footerLayout->addWidget(footerLabel_);
    mainLayout->addLayout(footerLayout);

    onRefreshIntervalChanged();
}

void CrmCrossRatesMatrixMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Reload the cross-rates matrix"));
    connect(reloadAction_, &QAction::triggered, this, &CrmCrossRatesMatrixMdiWindow::reload);

    toolbar_->addSeparator();

    exportCsvAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportCsv, IconUtils::DefaultIconColor),
        tr("Export CSV"));
    exportCsvAction_->setToolTip(tr("Export the displayed rates to CSV"));
    connect(
        exportCsvAction_, &QAction::triggered, this, &CrmCrossRatesMatrixMdiWindow::exportToCsv);

    exportOreAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportOre, IconUtils::DefaultIconColor),
        tr("Export ORE"));
    exportOreAction_->setToolTip(tr("Export the displayed rates as an ORE market data file"));
    connect(
        exportOreAction_, &QAction::triggered, this, &CrmCrossRatesMatrixMdiWindow::exportToOre);
}

void CrmCrossRatesMatrixMdiWindow::onRefreshIntervalChanged() {
    const int secs = refreshIntervalCombo_->currentData().toInt();
    if (secs <= 0) {
        BOOST_LOG_SEV(lg(), info) << "CRM matrix auto-refresh disabled";
        autoRefreshTimer_->stop();
        return;
    }
    autoRefreshTimer_->setInterval(secs * 1000);
    BOOST_LOG_SEV(lg(), info) << "CRM matrix auto-refresh interval=" << secs << "s";
    autoRefreshTimer_->start();
}

void CrmCrossRatesMatrixMdiWindow::onHideEmptyToggled(bool checked) {
    QSettings settings(QStringLiteral("OreStudio"), QStringLiteral("OreStudio"));
    settings.beginGroup(settingsGroup_);
    settings.setValue(QStringLiteral("hideEmpty"), checked);
    reload();
}

void CrmCrossRatesMatrixMdiWindow::onShowInvertedToggled(bool checked) {
    QSettings settings(QStringLiteral("OreStudio"), QStringLiteral("OreStudio"));
    settings.beginGroup(settingsGroup_);
    settings.setValue(QStringLiteral("showInverted"), checked);
    reload();
}

void CrmCrossRatesMatrixMdiWindow::onCellSelected(int row, int column) {
    // Dash cells (diagonal or no data) are plain QTableWidgetItems with no
    // click signal wired up -- only a real CrmRateCellWidget ever reaches
    // this slot -- so no same-currency/diagonal guard is needed here.
    if (row < 0 || column < 0)
        return;
    if (row >= static_cast<int>(displayedRowCurrencies_.size()) ||
        column >= static_cast<int>(displayedColumnCurrencies_.size()))
        return;

    updateOverviewPanel(displayedRowCurrencies_[row], displayedColumnCurrencies_[column]);
}

void CrmCrossRatesMatrixMdiWindow::updateOverviewPanel(const std::string& base,
                                                       const std::string& quote) {
    overviewPairLabel_->setText(QString::fromStdString(base) + QStringLiteral("/") +
                                QString::fromStdString(quote));

    const auto key = std::make_pair(base, quote);
    const auto history_it = rateHistory_.find(key);
    if (history_it == rateHistory_.end() || history_it->second.empty()) {
        overviewRateLabel_->setText(tr("No data"));
        overviewSparkline_->setValues({});
        return;
    }

    overviewRateLabel_->setText(
        tr("Rate: %1").arg(QString::number(history_it->second.back(), 'f', 5)));
    overviewSparkline_->setValues(history_it->second);
}

void CrmCrossRatesMatrixMdiWindow::reload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit statusChanged(tr("Not connected"));
        return;
    }

    reloadAction_->setEnabled(false);
    emit statusChanged(tr("Loading cross-rates matrix..."));

    const auto party_id = boost::uuids::to_string(clientManager_->currentPartyId());
    const auto crm_name = crmName_.toStdString();
    QPointer<CrmCrossRatesMatrixMdiWindow> self = this;
    const bool inverted = showInvertedButton_->isChecked();

    QFuture<RatesResult> future =
        QtConcurrent::run([self, party_id, crm_name, inverted]() -> RatesResult {
            if (!self || !self->clientManager_)
                return {};

            marketdata_msg::get_crm_rates_request req;
            req.party_id = party_id;
            req.crm_name = crm_name;
            req.inverted = inverted;
            auto resp = self->clientManager_->process_authenticated_request(req);

            RatesResult r;
            if (!resp) {
                r.error = QStringLiteral("Failed to communicate with server");
                return r;
            }
            if (!resp->success) {
                r.error = QString::fromStdString(resp->message);
                return r;
            }
            r.success = true;
            r.rates = resp->rates;
            return r;
        });

    auto* watcher = new QFutureWatcher<RatesResult>(this);
    connect(watcher, &QFutureWatcher<RatesResult>::finished, this, [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        self->reloadAction_->setEnabled(true);

        if (!result.success) {
            const QString msg =
                result.error.isEmpty() ? tr("Failed to load cross-rates matrix") : result.error;
            BOOST_LOG_SEV(lg(), error) << msg.toStdString();
            emit self->errorOccurred(msg);
            self->footerLabel_->setText(tr("DISCONNECTED"));
            return;
        }

        // Currencies are whatever the response actually covers, in
        // alphabetical order -- never a hardcoded/curated list.
        QSet<QString> currencySet;
        for (const auto& rate : result.rates) {
            currencySet.insert(QString::fromStdString(rate.base_currency_code));
            currencySet.insert(QString::fromStdString(rate.quote_currency_code));
        }
        QStringList allCurrencies(currencySet.begin(), currencySet.end());
        std::sort(allCurrencies.begin(), allCurrencies.end());

        // Populate the Base Currency filter from whatever currencies this
        // response actually covers, preserving the current selection.
        {
            const auto previous = self->baseCurrencyCombo_->currentData().toString();
            self->baseCurrencyCombo_->blockSignals(true);
            self->baseCurrencyCombo_->clear();
            self->baseCurrencyCombo_->addItem(tr("All"), QString());
            for (const auto& code : allCurrencies)
                self->baseCurrencyCombo_->addItem(code, code);
            const auto idx = self->baseCurrencyCombo_->findData(previous);
            self->baseCurrencyCombo_->setCurrentIndex(idx >= 0 ? idx : 0);
            self->baseCurrencyCombo_->blockSignals(false);
        }

        // Base Currency filter restricts the grid to a single row (that
        // currency as base against every other as quote), matching the
        // mockup's "Base Currency" selector while keeping the same
        // fixed-size-cell grid rendering path.
        const auto baseFilter = self->baseCurrencyCombo_->currentData().toString();
        QStringList rowCurrencies = baseFilter.isEmpty() ? allCurrencies : QStringList{baseFilter};

        std::map<std::pair<QString, QString>, marketdata_msg::crm_rate_item> byPair;
        for (const auto& rate : result.rates) {
            byPair[{QString::fromStdString(rate.base_currency_code),
                    QString::fromStdString(rate.quote_currency_code)}] = rate;
        }

        // "Hide Empty": drop rows/columns for currencies that never
        // appear as base/quote at all -- distinct from the per-cell
        // dash, which already covers "no data for this specific pair"
        // while the currency has data elsewhere (e.g. a USD-pivot-star
        // exotics CRM where ZAR is always a quote, never a base, so its
        // row is entirely dashes but its column isn't).
        if (self->hideEmptyButton_->isChecked()) {
            QSet<QString> basesSeen;
            QSet<QString> quotesSeen;
            for (const auto& rate : result.rates) {
                basesSeen.insert(QString::fromStdString(rate.base_currency_code));
                quotesSeen.insert(QString::fromStdString(rate.quote_currency_code));
            }
            QStringList filteredColumns;
            for (const auto& code : allCurrencies)
                if (quotesSeen.contains(code))
                    filteredColumns.push_back(code);
            allCurrencies = filteredColumns;

            QStringList filteredRows;
            for (const auto& code : rowCurrencies)
                if (basesSeen.contains(code))
                    filteredRows.push_back(code);
            rowCurrencies = filteredRows;
        }

        self->table_->clear();
        self->table_->setRowCount(rowCurrencies.size());
        self->table_->setColumnCount(allCurrencies.size());

        self->displayedRowCurrencies_.clear();
        for (const auto& code : rowCurrencies)
            self->displayedRowCurrencies_.push_back(code.toStdString());
        self->displayedColumnCurrencies_.clear();
        for (const auto& code : allCurrencies)
            self->displayedColumnCurrencies_.push_back(code.toStdString());

        for (int i = 0; i < allCurrencies.size(); ++i) {
            const auto code = allCurrencies[i].toStdString();
            const auto icon =
                self->imageCache_ ? currency_flag_icon(*self->imageCache_, code) : QIcon();
            auto* hHeader = new QTableWidgetItem(allCurrencies[i]);
            hHeader->setIcon(icon);
            self->table_->setHorizontalHeaderItem(i, hHeader);
        }
        for (int i = 0; i < rowCurrencies.size(); ++i) {
            const auto code = rowCurrencies[i].toStdString();
            const auto icon =
                self->imageCache_ ? currency_flag_icon(*self->imageCache_, code) : QIcon();
            auto* vHeader = new QTableWidgetItem(rowCurrencies[i]);
            vHeader->setIcon(icon);
            self->table_->setVerticalHeaderItem(i, vHeader);
        }

        std::vector<marketdata_msg::crm_rate_item> exportedRates;

        for (int row = 0; row < rowCurrencies.size(); ++row) {
            for (int col = 0; col < allCurrencies.size(); ++col) {
                if (rowCurrencies[row] == allCurrencies[col]) {
                    self->table_->setItem(row, col, make_dash_item());
                    continue;
                }

                const auto key = std::make_pair(rowCurrencies[row], allCurrencies[col]);
                const auto it = byPair.find(key);
                if (it == byPair.end()) {
                    self->table_->setItem(row, col, make_dash_item());
                    continue;
                }

                const auto& item = it->second;
                exportedRates.push_back(item);
                auto* cellWidget = new CrmRateCellWidget(self->table_);
                cellWidget->setPastelBackground(item.inverted);

                const auto display_key = std::make_pair(rowCurrencies[row].toStdString(),
                                                        allCurrencies[col].toStdString());
                const double display_rate = item.rate;

                // Only the %-change indicator is coloured (matching the
                // mockup); the rate itself stays neutral unless the cell
                // is stale/unavailable. Stale/unavailable also colour
                // the pair-code line, always paired with a distinct
                // glyph prefix (not colour alone) -- same convention as
                // FxSpotGridWindow's status icon+colour pairing.
                auto rateColor = color_constants::level_text;
                auto changeColor = color_constants::level_trace;
                auto pairColor = color_constants::level_trace;
                QString changeText = QStringLiteral("—");
                QString pairPrefix;
                QString tooltip;

                if (item.status == "stale") {
                    rateColor = color_constants::level_warn;
                    pairColor = color_constants::level_warn;
                    pairPrefix = QStringLiteral("⚠");
                    tooltip = tr("Stale as of %1").arg(QString::fromStdString(item.as_of));
                } else if (item.status == "unavailable") {
                    rateColor = color_constants::level_trace;
                    pairColor = color_constants::level_trace;
                    pairPrefix = QStringLiteral("✕");
                    tooltip = tr("Unavailable");
                } else {
                    tooltip = item.inverted ?
                                  tr("Computed inverse (1/rate); fresh as of %1")
                                      .arg(QString::fromStdString(item.as_of)) :
                                  tr("Fresh as of %1").arg(QString::fromStdString(item.as_of));
                    if (item.delta_pct.has_value() && std::abs(*item.delta_pct) > 1e-9) {
                        const auto pct = *item.delta_pct;
                        changeText = (pct >= 0 ? QStringLiteral("▲ +%1%") : QStringLiteral("▼ %1%"))
                                         .arg(QString::number(pct, 'f', 3));
                        changeColor =
                            pct >= 0 ? color_constants::level_info : color_constants::level_error;
                    }
                }

                cellWidget->setData(rowCurrencies[row] + QStringLiteral("/") + allCurrencies[col],
                                    QString::number(display_rate, 'f', 5),
                                    changeText,
                                    changeColor,
                                    rateColor,
                                    pairColor,
                                    pairPrefix);
                cellWidget->setToolTip(tooltip);
                connect(cellWidget, &CrmRateCellWidget::clicked, self, [self, row, col]() {
                    self->onCellSelected(row, col);
                });
                self->table_->setCellWidget(row, col, cellWidget);

                auto& history = self->rateHistory_[display_key];
                history.push_back(display_rate);
                while (history.size() > max_history_points)
                    history.pop_front();
            }
        }

        self->displayedRates_ = std::move(exportedRates);

        self->footerLabel_->setText(tr("CONNECTED | %1 Currencies").arg(allCurrencies.size()));

        BOOST_LOG_SEV(lg(), debug) << "CRM cross-rates matrix: " << result.rates.size()
                                   << " rate(s), " << allCurrencies.size() << " currencies";
        emit self->statusChanged(
            tr("Cross-rates matrix updated: %1 rate(s)").arg(result.rates.size()));
    });
    watcher->setFuture(future);
}

QString CrmCrossRatesMatrixMdiWindow::exportFileNameSlug() const {
    QStringList parts;
    parts << QStringLiteral("crm_rates");
    parts << (crmName_.isEmpty() ? QStringLiteral("all") : crmName_);

    const auto baseFilter = baseCurrencyCombo_->currentData().toString();
    if (!baseFilter.isEmpty())
        parts << baseFilter;

    if (showInvertedButton_->isChecked())
        parts << QStringLiteral("inverted");
    if (hideEmptyButton_->isChecked())
        parts << QStringLiteral("hideempty");

    return parts.join(QStringLiteral("_"));
}

void CrmCrossRatesMatrixMdiWindow::exportToCsv() {
    if (displayedRates_.empty()) {
        QMessageBox::information(this, tr("No Data"), tr("There are no rates to export."));
        return;
    }

    const QString fileName = QFileDialog::getSaveFileName(
        this,
        tr("Export to CSV"),
        QStringLiteral("%1_%2.csv").arg(exportFileNameSlug(), export_timestamp()),
        tr("CSV Files (*.csv);;All Files (*)"));
    if (fileName.isEmpty())
        return;

    try {
        std::ostringstream out;
        out << std::fixed << std::setprecision(5);
        out << "base,quote,rate,status,as_of\n";
        for (const auto& r : displayedRates_) {
            out << r.base_currency_code << ',' << r.quote_currency_code << ',' << r.rate << ','
                << r.status << ',' << r.as_of << '\n';
        }

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, tr("File Error"), tr("Could not open file for writing: %1").arg(fileName));
            return;
        }
        const auto csvData = out.str();
        file.write(csvData.c_str(), static_cast<qint64>(csvData.size()));
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
        emit statusChanged(tr("Successfully exported CRM rates to %1").arg(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, tr("Export Error"), tr("Error during CSV export: %1").arg(e.what()));
    }
}

void CrmCrossRatesMatrixMdiWindow::exportToOre() {
    if (displayedRates_.empty()) {
        QMessageBox::information(this, tr("No Data"), tr("There are no rates to export."));
        return;
    }

    const QString fileName = QFileDialog::getSaveFileName(
        this,
        tr("Export to ORE Market Data"),
        QStringLiteral("%1_%2.txt").arg(exportFileNameSlug(), export_timestamp()),
        tr("Text Files (*.txt);;All Files (*)"));
    if (fileName.isEmpty())
        return;

    std::vector<ores::ore::market::market_datum> data;
    data.reserve(displayedRates_.size());
    for (const auto& r : displayedRates_) {
        if (r.status == "unavailable")
            continue; // no rate value to export

        ores::ore::market::market_datum datum;
        datum.date = parse_as_of_date(r.as_of);
        datum.key = "FX/RATE/" + r.base_currency_code + "/" + r.quote_currency_code;
        datum.value = std::to_string(r.rate);
        data.push_back(std::move(datum));
    }

    if (data.empty()) {
        QMessageBox::information(
            this, tr("No Data"), tr("There are no fresh/stale rates to export."));
        return;
    }

    try {
        std::ostringstream out;
        ores::ore::market::serialize_market_data(out, data);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, tr("File Error"), tr("Could not open file for writing: %1").arg(fileName));
            return;
        }
        const auto textData = out.str();
        file.write(textData.c_str(), static_cast<qint64>(textData.size()));
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, tr("Export Error"), tr("Error during ORE market data export: %1").arg(e.what()));
        return;
    }

    emit statusChanged(tr("Successfully exported CRM rates to %1").arg(fileName));
}

}
