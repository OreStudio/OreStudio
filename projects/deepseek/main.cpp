#include <iostream>
#include <boost/asio.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/bind_cancellation_slot.hpp>
#include <memory>
#include <vector>
#include <syncstream>  // C++23 synchronized output

namespace asio = boost::asio;
using asio::ip::tcp;
using asio::cancellation_type;
using asio::cancellation_slot;
using asio::cancellation_signal;

class session : public std::enable_shared_from_this<session>
{
public:
    session(tcp::socket socket, cancellation_signal& cancel_signal)
        : socket_(std::move(socket))
        , cancel_signal_(cancel_signal)
    {
        // Connect this session to the global cancellation signal
        slot_ = cancel_signal_.slot();
        slot_.assign([this](cancellation_type type) {
            handle_cancellation(type);
        });
    }

    ~session()
    {
        // Clean up the slot when session is destroyed
        if (slot_.is_connected()) {
            slot_.clear();
        }
    }

    void start()
    {
        active_ = true;
        do_read();
    }

private:
    void handle_cancellation(cancellation_type type)
    {
        if (active_) {
            active_ = false;
            std::osyncstream(std::cout)
                << "Session " << this << " received cancellation type: "
                << static_cast<int>(type) << "\n";

            boost::system::error_code ec;
            socket_.cancel(ec);

            // Close the socket completely
            socket_.close(ec);
        }
    }

    void do_read()
    {
        if (!active_) return;

        auto self(shared_from_this());
        socket_.async_read_some(
            asio::buffer(data_),
            [this, self](boost::system::error_code ec, std::size_t length)
            {
                if (!ec && active_) {
                    // Process data...
                    std::osyncstream(std::cout)
                        << "Session " << this << " received " << length << " bytes\n";
                    do_read();
                } else if (ec) {
                    std::osyncstream(std::cout)
                        << "Session " << this << " error: " << ec.message() << "\n";
                }
            });
    }

    tcp::socket socket_;
    std::array<char, 1024> data_{};
    cancellation_signal& cancel_signal_;
    cancellation_slot slot_;
    bool active_{false};
};

class server
{
public:
    server(asio::any_io_executor executor, short port)
        : acceptor_(executor, tcp::endpoint(tcp::v4(), port))
        , signals_(executor)
        , global_cancel_signal_()
    {
        setup_signal_handling();
        do_accept();
    }

    void cancel_all_sessions()
    {
        std::osyncstream(std::cout)
            << "Emitting cancellation to all sessions\n";

        // Emit cancellation signal - this will trigger all connected slots
        global_cancel_signal_.emit(cancellation_type::all);

        // Optional: Clear sessions list after some delay
        // Using a timer to allow graceful cleanup
        auto timer = std::make_shared<asio::steady_timer>(acceptor_.get_executor());
        timer->expires_after(std::chrono::seconds(2));
        timer->async_wait([this, timer](boost::system::error_code) {
            std::osyncstream(std::cout)
                << "Cleaning up " << sessions_.size() << " sessions\n";
            sessions_.clear();
        });
    }

private:
    void setup_signal_handling()
    {
        // Register for common termination signals
        signals_.add(SIGINT);
        signals_.add(SIGTERM);
#if defined(SIGQUIT)
        signals_.add(SIGQUIT);
#endif

        // Async wait for signals
        signals_.async_wait([this](boost::system::error_code ec, int signal_number) {
            handle_signal(ec, signal_number);
        });
    }

    void handle_signal(boost::system::error_code ec, int signal_number)
    {
        if (!ec) {
            std::osyncstream(std::cout)
                << "Received signal " << signal_number
                << ", initiating shutdown\n";

            // First, stop accepting new connections
            acceptor_.close();

            // Then cancel all existing sessions
            cancel_all_sessions();
        }
    }

    void do_accept()
    {
        acceptor_.async_accept(
            asio::bind_cancellation_slot(
                global_cancel_signal_.slot(),
                [this](boost::system::error_code ec, tcp::socket socket)
                {
                    if (!ec) {
                        std::osyncstream(std::cout)
                            << "New connection accepted\n";

                        // Create new session with reference to cancellation signal
                        auto new_session = std::make_shared<session>(
                            std::move(socket),
                            global_cancel_signal_
                        );
                        sessions_.push_back(new_session);
                        new_session->start();

                        // Continue accepting new connections
                        do_accept();
                    } else if (ec != asio::error::operation_aborted) {
                        std::osyncstream(std::cout)
                            << "Accept error: " << ec.message() << "\n";
                    }
                }));
    }

    tcp::acceptor acceptor_;
    asio::signal_set signals_;
    cancellation_signal global_cancel_signal_;
    std::vector<std::shared_ptr<session>> sessions_;
};

// Alternative: Session with its own cancellation signal
class session_with_local_cancel : public std::enable_shared_from_this<session_with_local_cancel>
{
public:
    session_with_local_cancel(tcp::socket socket, cancellation_signal& global_signal)
        : socket_(std::move(socket))
        , local_cancel_signal_()
    {
        // Connect local signal to global signal
        auto slot = global_signal.slot();
        slot.assign([this](cancellation_type type) {
            // Forward cancellation to local signal
            local_cancel_signal_.emit(type);
        });
    }

    void start()
    {
        do_read_with_local_cancellation();
    }

private:
    void do_read_with_local_cancellation()
    {
        auto self(shared_from_this());

        socket_.async_read_some(
            asio::buffer(data_),
            asio::bind_cancellation_slot(
                local_cancel_signal_.slot(),
                [this, self](boost::system::error_code ec, std::size_t length)
                {
                    if (ec == asio::error::operation_aborted) {
                        std::osyncstream(std::cout)
                            << "Read operation cancelled via cancellation slot\n";
                        return;
                    }

                    if (!ec) {
                        std::osyncstream(std::cout)
                            << "Read " << length << " bytes\n";
                        do_read_with_local_cancellation();
                    }
                }));
    }

    tcp::socket socket_;
    std::array<char, 1024> data_{};
    cancellation_signal local_cancel_signal_;
};

int main()
{
    asio::io_context io_context;

    // Create server with port 8080
    server s(io_context.get_executor(), 8080);

    std::osyncstream(std::cout) << "Server started on port 8080\n";
    std::osyncstream(std::cout) << "Press Ctrl+C to shutdown\n";

    io_context.run();

    std::osyncstream(std::cout) << "Server shutdown complete\n";

    return 0;
}
